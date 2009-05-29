/* ex: set ts=2 et: */
/* $Id$ */
/*
 * front-end to exec code with limits on time and resource usage;
 * protection of the underlying machine still relies on the language
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <errno.h>

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

/*
 * if child exits before sleep over our job is done
 */
static void sig(int sig)
{
  if (SIGCHLD == sig) {
    exit(0);
  }
}

static void do_fork(char * const argv[])
{
  pid_t child = fork();
  if (child < 0) { /* error */
    perror("fork");
    exit(1);
  } else if (0 == child) { /* child */
    int e = execv(argv[1], argv+1);
    if (e) {
      perror("execl");
    }
    fprintf(stderr, "child done (e=%d)\n", e);
    exit(0);
  } else { /* parent */
    pid_t w;
    int status = 0;
    /* sleep N time */
    sleep(1);
    /* if we haven't gotten a "child died" signal in that time... */
    w = waitpid(child, &status, WNOHANG);
    fprintf(stderr, "w=%d errno=%d\n", w, errno);
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
  int i;
  assert((char *)NULL == argv[argc]);
  for (i = 0; i < argc; i++) {
    printf("argv[%d]: %s\n", i, argv[i]);
  }
  printf("argv[%d]: %s\n", i, argv[i]);
  /* set up signal handler */
  (void)signal(SIGCHLD, &sig);
  /* fork */
  do_fork(argv);
  return 0;
}

