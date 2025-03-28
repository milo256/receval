CC := gcc
ASAN := 1
RELEASE := 0
APP_NAME := receval

CFLAGS := -std=c99 -Wall -Wextra -Wno-missing-braces
LDFLAGS :=
DEBUGFLAGS := -g -O0
RELEASEFLAGS := -g -O0


SRCS := receval.c parser.c tokenizer.c utils.c
HEADERS := common.h ident.h arena.h da.h tokenizer.h parser.h
OBJDIR := obj
OBJS = $(patsubst %.c, $(OBJDIR)/%.o, $(SRCS))


ifeq ($(RELEASE), 1)
	CFLAGS += $(RELEASEFLAGS)
else
	CFLAGS += $(DEBUGFLAGS)
ifeq ($(ASAN), 1)
	CFLAGS += -fsanitize=address
	LDFLAGS += -fsanitize=address
endif
endif

all: $(OBJS)
	$(CC) $(LDFLAGS) $^ -o $(APP_NAME)

run: all
	./$(APP_NAME)

$(OBJDIR)/%.o: %.c $(HEADERS)
	mkdir -p $(OBJDIR)
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm $(APP_NAME) $(OBJDIR)/*.o


.PHONY: clean
