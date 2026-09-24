/* Values narrower than a register crossing the boundary.
 *
 * The psABI says a scalar narrower than the register is widened to
 * thirty-two bits by the sign of its type and then sign-extended, so an
 * unsigned short is passed zero-extended where a signed one is passed
 * sign-extended.  A function compiled by another compiler relies on it:
 * glibc's htons shifts the whole register, so a uint16_t handed to it
 * sign-extended comes back wrong -- which is how this was found, by way
 * of OCaml's socket tests failing to connect to a port they had just
 * bound.
 *
 * These call into the C library, so the boundary is a real one however
 * this file is compiled. */
#include <arpa/inet.h>
#include <string.h>
int puts(const char *);
static void yes(const char *w, int ok) { puts(ok ? w : "FAILED"); }

static unsigned short ours_us(unsigned short x) { return (unsigned short)(x + 1); }
static unsigned char ours_uc(unsigned char x) { return (unsigned char)(x + 1); }
static short ours_ss(short x) { return (short)(x - 1); }

int main(void) {
  volatile unsigned short v = 0xCC47;          /* the top bit set */
  yes("a byte swap and back", ntohs(htons(v)) == 0xCC47);
  yes("the swap itself", htons((unsigned short)0xCC47) == 0x47CC);
  yes("one with every bit set", ntohs(htons((unsigned short)0xFFFF)) == 0xFFFF);
  yes("a small one", ntohs(htons((unsigned short)1)) == 1);
  yes("four bytes", ntohl(htonl(0xDEADBEEFu)) == 0xDEADBEEFu);

  /* our own functions, where both sides are this compiler */
  volatile unsigned short u = 0xFFFE;
  volatile unsigned char c = 0xFE;
  volatile short s = -32768;
  yes("an unsigned short of ours", ours_us(u) == 0xFFFF);
  yes("an unsigned char of ours", ours_uc(c) == 0xFF);
  yes("a signed short of ours", ours_ss(s) == 32767);
  yes("one that wraps", ours_us((unsigned short)0xFFFF) == 0);

  /* a narrow argument to the C library */
  char buf[8];
  memset(buf, 0xC7, sizeof buf);
  yes("memset with a high byte", (unsigned char)buf[0] == 0xC7);
  yes("memchr finds it", memchr(buf, 0xC7, 8) == buf);
  return 0;
}
