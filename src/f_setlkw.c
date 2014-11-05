/* A simple C program which blocks until a given file is lockable.
 *
 * Designed to be used after sending the stop event to a cl-service
 * server so that we can wait until the service is actually down.
 */

#include <unistd.h>
#include <stdio.h>

#include <unistd.h>
#include <fcntl.h>

#include <errno.h>
#include <stdlib.h>
#include <sysexits.h>

void usage() {
  fprintf(stderr, "Usage: f_setlkw [ -t ] FILENAME");
  _exit(EX_USAGE);
}

int main(int argc, char* argv[]) {

  int test_lock = 0;

  int c;
  while ((c = getopt(argc, argv, "th")) != -1) {
    switch(c) {
    case 't':
      test_lock = 1;
      break;
    case 'h':
      usage();
      break;
    default:
      printf("?? getopt returned code %d\n", c);
      _exit(2);
    }
  }

  char* filename = argv[optind];
  if (filename == NULL) {
    usage();
  }

  int fd = open(filename, O_WRONLY);
  if (fd < 0) {
    if (errno == ENOENT) {
      fprintf(stderr, "File %s does not exist.\n", filename);
      _exit(EX_USAGE);
    }
  }
    
  struct flock fl;
  fl.l_type = F_WRLCK;
  fl.l_whence = SEEK_SET;
  fl.l_start = 0;
  fl.l_len = 0;

  if (test_lock) {
    int ret = fcntl(fd, F_GETLK, &fl);
    if (ret == -1) {
      _exit(2);
    } else if (fl.l_type == F_UNLCK) {
      // file is lockable
      _exit(0);
    } else {
      // someone holds the lock.
      _exit(1);
    }
  } else {
    int ret = fcntl(fd, F_SETLKW, &fl);
    if (ret == -1 && (errno == EINTR)) {
      _exit(1);
    } else {
      _exit(0);
    }
  }
}
