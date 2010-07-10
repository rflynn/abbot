/* ex: set ts=2 et: */
/* $Id$ */
/*
 * exec() cmdline parameters with a strict time-limit.
 * meant for running code samples and returning the result.
 * I'm sure it's a horrible security hazard.
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <errno.h>
#include <signal.h>

/*
 * the effectiveness of this obviously
 * depends on the capabilities and load
 * on the current system
 */
#define SLEEP_MSEC 400

/**
 * sleep()'s resolution is too coarse and
 * nanosleep() isn't available everywhere...
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

