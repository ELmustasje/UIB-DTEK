#include <stdio.h>  // getline
#include <stdlib.h> // exit
#include <string.h> // strtok
#include <sys/wait.h>
#include <time.h>
#include <unistd.h> // execvp

#define MAX_ARGS 256

size_t n = 0;
char *line = NULL;
char *args[MAX_ARGS];

/*
    Reads one line from the standard input and
    splits it into tokens. The resulting tokens
    are stored args, terminated by a NULL pointer.
*/
void parse_command_from_user() {
  int rc = getline(&line, &n, stdin);
  if (rc < 0) // Close the shell at end-of-input
    exit(0);

  int i = 0;
  args[i++] = strtok(line, " \n");
  while (i < MAX_ARGS && args[i - 1] != NULL)
    args[i++] = strtok(NULL, " \n");
}

pid_t child_pid = -1;
void sigint_handler(int sig) {
  if (child_pid > 0) {
    kill(child_pid, SIGINT);
  }
}

int main() {
  struct sigaction sa;
  sa.sa_handler = sigint_handler;
  sa.sa_flags = 0;
  sigemptyset(&sa.sa_mask);
  sigaction(SIGINT, &sa, NULL);

  while (1) {
    printf(">");
    fflush(stdout);
    parse_command_from_user();
    if (args[0] == NULL) {
      continue;
    }
    if (strcmp(args[0], "exit") == 0) {
      break;
    }
    int pid = fork();
    if (pid == 0) {
      execvp(args[0], args);
      exit(0);
    } else {
      wait(NULL);
      child_pid = -1;
    }
  }
  return 0;
}
