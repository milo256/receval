CC := gcc
CFLAGS := -Wall -Wextra -g -O0
APP_NAME := receval2

all:
	$(CC) $(CFLAGS) $(APP_NAME).c -o $(APP_NAME)