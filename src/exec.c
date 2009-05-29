/* ex: set ts=2 et: */
/* $Id$ */
/*
 * front-end to exec code with limits on time and resource usage;
 * protection of the underlying machine still relies on the language
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <errno.h>

#define SLEEP_MSEC 300

/*
pizza@anchovie:~/proj/abbot/src$ time ./exec /usr/bin/ruby "-e puts Thread.start{ \$SAFE=4; [1,2,3].collect{|i|i*i} }.join.value.inspect"
argv[0]: ./exec
argv[1]: /usr/bin/ruby
argv[2]: -e puts Thread.start{ $SAFE=4; [1,2,3].collect{|i|i*i} }.join.value.inspect
argv[3]: (null)
[1, 4, 9]
real    0m0.083s
user    0m0.000s
sys     0m0.008s
*/

/**
 * sleep()'s resolution is too coarse; nanosleep() isn't available everywhere...
 */
static void do_sleep()
{
  struct timeval tv;
  tv.tv_sec = 0;
  tv.tv_usec = SLEEP_MSEC * 1000;
  (void)select(1, NULL, NULL, NULL, &tv);
}

/*
 * if child exits before sleep over our job is done
 */
static void sig(int sig)
{
  if (SIGCHLD == sig)
    exit(0);
}

static void do_fork(char * const argv[])
{
  pid_t child = fork();
  if (child < 0) { /* error */
    perror("fork");
    exit(1);
  } else if (0 == child) { /* child */
    int e = execv(argv[1], argv+1);
    if (e)
      perror("execl");
    exit(0);
  } else { /* parent */
    pid_t w;
    int status = 0;
    do_sleep();
    /* if we haven't gotten a "child died" signal in that time... */
    w = waitpid(child, &status, WNOHANG);
    if (0 == w) {
      /* child hasn't exited already(!) */
      /* kill naughty child */
      kill(child, SIGKILL);
      /* return timeout error */
      printf("timeout\n");
      exit(1);
    }
    exit(0);
  }
}

int main(int argc, char *argv[])
{
  assert((char *)NULL == argv[argc]);
  (void)signal(SIGCHLD, &sig);
  do_fork(argv);
  return 0;
}

