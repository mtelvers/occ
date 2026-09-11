/* a thread-local nothing else can see, and one it can */
static _Thread_local long hidden = 11;
_Thread_local long shown = 22;
long bump(void) { hidden += 1; shown += 1; return hidden * 100 + shown; }
