/* names two things it does not define: whatever loads it will have them */
extern void host_says(const char *);
extern int host_counter;
void run(void) { host_says("hello from the plugin"); host_counter += 10; }
