#include "mmio.h"

/* Define the MMIO register addresses */
#define VECADD_STATUS 0x2000
#define VECADD_X 0x2004
#define VECADD_Y 0x2008
#define VECADD_RES 0x200C

unsigned int VECADD_ref(unsigned int x, unsigned int y) {
 /* TODO: fill our the C reference solution*/
}

// DOC include start: VECADD test
int main(void)
{
  uint32_t result, ref, x = 20, y = 15;

  /* wait for peripheral to be ready i.e. wait until we can write to the MMIOs*/
  while ((reg_read8(VECADD_STATUS) & 0x2) == 0) ;

  /* write our input values to MMIOs*/
  reg_write32(VECADD_X, x);
  reg_write32(VECADD_Y, y);


  /* wait for peripheral to complete i.e. wait until we have the result*/
  while ((reg_read8(VECADD_STATUS) & 0x1) == 0) ;

  /* read the result */
  result = reg_read32(VECADD_RES);

  /* compare the result with the refence C solution */
  ref = VECADD_ref(x, y);

  if (result != ref) {
    printf("Hardware result %d does not match reference value %d\n", result, ref);
    return 1;
  }
  printf("Hardware result %d is correct for VECADD\n", result);
  return 0;
}
// DOC include end: VECADD test
