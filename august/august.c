// info: http://thiemonge.org/getting-started-with-uinput

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>

#include <linux/input.h>
#include <linux/uinput.h>

#define SIZE 8


int uinput_sync(int fd) 
{
      struct input_event ev = {0};

      ev.type = EV_SYN;
      return write(fd, &ev, sizeof(struct input_event));
}

int uinput_keypress(int fd, __u16 code) 
{
      struct input_event ev = {0};

      ev.type = EV_KEY;
      ev.code = code;
      ev.value = 1;
      return write(fd, &ev, sizeof(ev));
}

int uinput_keyrelease(int fd, __u16 code) 
{
      struct input_event ev = {0};

      ev.type = EV_KEY;
      ev.code = code;
      ev.value = 0;
      return write(fd, &ev, sizeof(ev));
}

int main(int argc, char **argv)
{
  if (argc < 2) {
    fprintf(stderr, "Filename needed.\n");
    exit(1);
  }

  int uifd = open("/dev/uinput", O_WRONLY | O_NONBLOCK | O_SYNC);
  if(uifd < 0) {
    perror("Failed to open /dev/uinput");
    exit(EXIT_FAILURE);
  }

  //FILE *fp = fopen (argv[1], "rb");
  int dev = open(argv[1], O_RDONLY);

  if (dev < 0) {
    perror("Failed to open file");
    exit(1);
  }

  // set up uinput
  int res = ioctl(uifd, UI_SET_EVBIT, EV_SYN);
  fprintf(stderr, "%d: %d\n", __LINE__, res);
  res = ioctl(uifd, UI_SET_EVBIT, EV_KEY);
  fprintf(stderr, "%d: %d\n", __LINE__, res);
  res = ioctl(uifd, UI_SET_KEYBIT, KEY_PAGEDOWN);
  fprintf(stderr, "%d: %d\n", __LINE__, res);
  res = ioctl(uifd, UI_SET_KEYBIT, KEY_PAGEUP);
  fprintf(stderr, "%d: %d\n", __LINE__, res);

  struct uinput_user_dev uidev = {0};
  snprintf(uidev.name, UINPUT_MAX_NAME_SIZE, "august-uinput-handler");
  uidev.id.bustype = BUS_USB;
  uidev.id.vendor  = 0x1;
  uidev.id.product = 0x1;
  uidev.id.version = 1;

  res = write(uifd, &uidev, sizeof(uidev));
  fprintf(stderr, "%d: %d\n", __LINE__, res);
  res = ioctl(uifd, UI_DEV_CREATE);
  fprintf(stderr, "%d: %d\n", __LINE__, res);

  struct input_event ev = {0};
  
  int c;
  int pressed = 0;
  unsigned char buf[SIZE];
  unsigned char patt_prev[SIZE] = {0, 0, 0x4b, 0, 0, 0, 0, 0};
  unsigned char patt_next[SIZE] = {0, 0, 0x4e, 0, 0, 0, 0, 0};
  unsigned char patt_rel[SIZE] = { 0 };
  

  //while (fread(buf, SIZE, 1, fp) > 0) {
  while ((res = read(dev, buf, SIZE)) > 0) {
    fprintf(stderr, "%02x%02x%02x%02x%02x%02x%02x%02x  ", 
        buf[0], buf[1], buf[2], buf[3], buf[4], buf[5], buf[6], buf[7]);

    if (memcmp(buf, patt_prev, SIZE) == 0) {
      fprintf(stderr, "Prev\n");
      pressed = KEY_PAGEUP;
      res = uinput_keypress(uifd, pressed);
      fprintf(stderr, "%d: %d\n", __LINE__, res);
      res = uinput_sync(uifd);
      fprintf(stderr, "%d: %d\n", __LINE__, res);
    }
    else if (memcmp(buf, patt_next, SIZE) == 0) {
      fprintf(stderr, "Next\n");
      pressed = KEY_PAGEDOWN;
      res = uinput_keypress(uifd, pressed);
      fprintf(stderr, "%d: %d\n", __LINE__, res);
      res = uinput_sync(uifd);
      fprintf(stderr, "%d: %d\n", __LINE__, res);
    }
    else if ((memcmp(buf, patt_rel, SIZE) == 0) && pressed) {
      fprintf(stderr, "Release\n");
      res = uinput_keyrelease(uifd, pressed);
      fprintf(stderr, "%d: %d\n", __LINE__, res);
      res = uinput_sync(uifd);
      fprintf(stderr, "%d: %d\n", __LINE__, res);
    }
    else {
      fprintf(stderr, "Other\n");
    }
  }

//  if (ferror(fp)) {
  if (res < 0) {
    perror("I/O Error");
  }
 
  //fclose(fp);
  close(dev);
  res = ioctl(uifd, UI_DEV_DESTROY);
  close(uifd);
  return 0;
}
