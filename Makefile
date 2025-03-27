CC := gcc
CFLAGS := -std=c99 -Wall -Wextra -g -O0 -fsanitize=address ASAN_OPTIONS=detect_leaks=0
LDFLAGS := -fsanitize=address 
APP_NAME := receval

SRCS := receval.c parse.c tokenizer.c
HEADERS := tokenizer.h common.h da.h parse.h
OBJDIR := obj
OBJS = $(patsubst %.c, $(OBJDIR)/%.o, $(SRCS))

all: $(OBJS)
	$(CC) $(LDFLAGS) $^ -o $(APP_NAME)

run: all
	./$(APP_NAME)

$(OBJDIR)/%.o: %.c $(HEADERS)
	mkdir -p $(OBJDIR)
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm $(APP_NAME) *.o


.PHONY: clean
