/* Source: http://web.archive.org/web/20121102023700/http://www.concentric.net/~Ttwang/tech/inthash.htm */

import java.util.*;
import java.io.*;
public class testchange
{
  static class mwcGenerator
  {
    int s1;
    int s2;
    public mwcGenerator(int v1, int v2) { s1=v1; s2=v2; }
    public mwcGenerator() {  this(0xe43e9281, 0x3eb0926c);  }
    public int nextInt()
    {
      // MWC generator, period length 1014595583
      final int v1 = s1;
      final int v2 = s2;
      return ((s1 = 36969 * (v1 & 0xffff) + (v1 >>> 16)) << 16) ^
        (s2 = 30963 * (v2 & 0xffff) + (v2 >>> 16));
    }
  }

static class mt19937ar
{
/* 
   A C-program for MT19937, with initialization improved 2002/1/26.
   Coded by Takuji Nishimura and Makoto Matsumoto.

   Before using, initialize the state by using init_genrand(seed)  
   or init_by_array(init_key, key_length).

   Copyright (C) 1997 - 2002, Makoto Matsumoto and Takuji Nishimura,
   All rights reserved.                          

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

     1. Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.

     2. Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.

     3. The names of its contributors may not be used to endorse or promote 
        products derived from this software without specific prior written 
        permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


   Any feedback is very welcome.
   http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html
   email: m-mat @ math.sci.hiroshima-u.ac.jp (remove space)
*/
  // Period parameters
  public static final int N = 624;
  static final int M = 397;
  static final int MATRIX_A = 0x9908b0df; // constant vector a
  static final int UPPER_MASK = 0x80000000; // most significant w-r bits
  static final int LOWER_MASK = 0x7fffffff; // least significant r bits

  final int[] mt = new int[N]; // the array for the state vector
  int mti = N + 1; // mti == N+1 means mt[N] is not initialized

  /**
   * Initializes mt[N] with a seed
   */
  public mt19937ar(int s)
  {
    mt[0]= s;
    for (mti=1; mti<N; mti++) {
        mt[mti] = 
	    (1812433253 * (mt[mti-1] ^ (mt[mti-1] >>> 30)) + mti); 
        /* See Knuth TAOCP Vol2. 3rd Ed. P.106 for multiplier. */
        /* In the previous versions, MSBs of the seed affect   */
        /* only MSBs of the array mt[].                        */
        /* 2002/01/09 modified by Makoto Matsumoto             */
    }
  }

  /**
   * Initialize by an array with array-length 
   * @param init_key the array for initializing keys
   */
  public mt19937ar(int init_key[])
  {
    /* slight change for C++, 2004/2/26 */
    this(19650218);
    int key_length = init_key.length;
    int i, j, k;
    i=1; j=0;
    k = (N>key_length ? N : key_length);
    for (; k != 0; k--) {
        mt[i] = (mt[i] ^ ((mt[i-1] ^ (mt[i-1] >>> 30)) * 1664525))
          + init_key[j] + j; /* non linear */
        i++; j++;
        if (i>=N) { mt[0] = mt[N-1]; i=1; }
        if (j>=key_length) j=0;
    }
    for (k=N-1; k != 0; k--) {
        mt[i] = (mt[i] ^ ((mt[i-1] ^ (mt[i-1] >>> 30)) * 1566083941))
          - i; /* non linear */
        i++;
        if (i>=N) { mt[0] = mt[N-1]; i=1; }
    }
    mt[0] = 0x80000000; /* MSB is 1; assuring non-zero initial array */ 
  }

  /**
   * Default constructor
   */
  public mt19937ar()
  {
    this(5489);
  }

  /**
   * Generates a random number on [0,0xffffffff]-interval
   */
  public int nextInt()
  {
    int y;
    if (mti >= N) { /* generate N words at one time */
        int kk = 0;
        for (;kk<N-M;++kk) {
            y = (mt[kk]&UPPER_MASK)|(mt[kk+1]&LOWER_MASK);
            mt[kk] = mt[kk+M] ^ (y >>> 1) ^ (((y&1)>0) ? MATRIX_A : 0);
        }
        for (;kk<N-1;++kk) {
            y = (mt[kk]&UPPER_MASK)|(mt[kk+1]&LOWER_MASK);
            mt[kk] = mt[kk+(M-N)] ^ (y >>> 1) ^ (((y&1)>0) ? MATRIX_A : 0);
        }
        y = (mt[N-1]&UPPER_MASK)|(mt[0]&LOWER_MASK);
        mt[N-1] = mt[M-1] ^ (y >>> 1) ^ (((y&1)>0) ? MATRIX_A : 0);
        mti = 0;
    }  
    y = mt[mti++];
    // Tempering
    y ^= (y >>> 11);
    y ^= (y << 7)  & 0x9d2c5680;
    y ^= (y << 15) & 0xefc60000;
    y ^= (y >>> 18);
    return y;
  }

  public static void main(String[] args)
  {
    int init[] = { 0x123, 0x234, 0x345, 0x456 };
    mt19937ar ran = new mt19937ar(init);
    for (int i = 0; i < 1000; ++i)
    {
      System.out.print(ran.nextInt());
      System.out.print(' ');
      if (i%5==4) System.out.println();
    }
  }

}
  static int[] recBox = new int[64 * 64];
  public static void clearRecBox(int rsize, int csize)
  {
    for (int i = rsize * csize; --i >= 0; ) recBox[i] = 0;
  }
  public static int bumpUpRecBox(int r, int c, int csize)
  {
    return ++recBox[r * csize + c];
  }
  public static int getMinEntry(int r, int csize)
  {
    int p = r * csize;
    int min = recBox[p];
    for (int i = csize; --i >= 1; )
    {
      if (recBox[p + i] < min) min = recBox[p + i];
    }
    return min;
  }
  public static int getMaxEntry(int r, int csize)
  {
    int p = r * csize;
    int max = recBox[p];
    for (int i = csize; --i >= 1; )
    {
      if (recBox[p + i] > max) max = recBox[p + i];
    }
    return max;
  }
  static mt19937ar mtGen;
  static
  {
    int[] buf = new int[mt19937ar.N];
    mwcGenerator tmpgen = new mwcGenerator();
    for (int i = buf.length; --i >= 0; ) buf[i] = tmpgen.nextInt();
    mtGen = new mt19937ar(buf);
  }
  public static int nextInt() { return mtGen.nextInt(); }
  public static long nextLong()
  {
    return (((long) nextInt()) << 32) + nextInt();
  }
  public static float nextFloat()
  {
    return 5.9604645e-8f * (0x00ffffff & nextInt());
  }
  public static double nextDouble()
  {
    return 2.22044604925031e-16 * (0x000fffffffffffffL & nextLong());
  }
  static interface hashable32
  {
    public int hash(int arg, int aux, int a2,int a3,
    int a4,int a5,int a6,int a7,int a8);
  }
  static interface hashable64
  {
    public long hash(long arg, long aux, int a2,int a3,
    int a4,int a5,int a6,int a7,int a8);
  }
  static interface hashable6432
  {
    public int hash(long arg, long aux, int a2,int a3,
    int a4,int a5,int a6,int a7,int a8);
  }
  static class algo9 implements hashable64
  {
    public long hash(long arg,long aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      arg =  (arg << a2) - arg - 1;  // 2
      arg = (arg ) ^ (arg >>> a3);  // 2
      arg = (arg + (arg << 2)) + (arg << a4);  // 2 or 1
      arg = arg ^ (arg >>> a5);    // 2
      arg = (arg  + (arg << 3) ) + (arg << a6);   // 2
      arg = arg ^ (arg >>> a7);    // 2
      arg = (arg) + (arg << a8);  // 2
      return arg; // 14 to 13
    }
  }
  static class hardalgo9a implements hashable64
  {
    public long hash(long arg,long aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      arg = (arg << 21) - arg - 1;  // 2
      arg = (arg ) ^ (arg >>> 24);  // 2
      arg = (arg * 265);  // 2 or 1
      arg = arg ^ (arg >>> 14);    // 2
      arg = (arg * 21);   // 2
      arg = arg ^ (arg >>> 28);    // 2
      arg = (arg) + (arg << 31);  // 2
      return arg; // 14 to 13
    }
  }
  static class algo9a implements hashable64
  {
    public long hash(long arg,long aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      arg = (arg << a2) - arg - 1;  // 2
      arg = (arg ) ^ (arg >>> a3);  // 2
      arg = (arg + (arg << 3)) + (arg << a4);  // 2 or 1
      arg = arg ^ (arg >>> a5);    // 2
      arg = (arg  + (arg << 2) ) + (arg << a6);   // 2
      arg = arg ^ (arg >>> a7);    // 2
      arg = (arg) + (arg << a8);  // 2
      return arg; // 14 to 13
    }
  }
  static class hardalgo9 implements hashable64
  {
    public long hash(long arg,long aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      arg = (arg << 21) - arg - 1; // 2
      arg = (arg) ^ (arg >>> 25); // 2
      arg = (arg) + (arg << 6);  // 1 or 2
      arg = arg ^ (arg >>> 13);   // 2
      arg = (arg + (arg << 2)) + (arg << 4);   // 2
      arg = arg ^ (arg >>> 29);   // 2
      arg = (arg + (arg << 3) ) + (arg << 28); // 2
      return arg; // 13 to 12
    }
  }
  static class algo10 implements hashable64
  {
    public long hash(long arg,long aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      arg = (~arg) + (arg << a2); // 2
      arg = arg ^ (arg >>> a3);   // 2
      arg = (arg) + (arg << a4);  // 1 or 2
      arg = arg ^ (arg >>> a5);   // 2
      arg = arg + (arg << 3);     // 1 or 2
      arg = arg ^ (arg >>> a6);   // 2
      arg = (arg) + (arg << a7);  // 2
      arg = arg ^ (arg >>> a8);   // 2
      return arg; // 16 or 15
    }
  }
  static class hardalgo10 implements hashable64
  {
    public long hash(long arg,long aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      arg = (~arg) + (arg << 30); // 2
      arg = arg ^ (arg >>> 24);   // 2
      arg = (arg) + (arg << 7);  // 1 or 2
      arg = arg ^ (arg >>> 11);    // 2
      arg = arg + (arg << 3);     // 1 or 2
      arg = arg ^ (arg >>> 5);   // 2
      arg = (arg) + (arg << 25);  // 2
      arg = arg ^ (arg >>> 29);   // 2
      return arg; // 16 or 14
    }
  }
  static class algo11 implements hashable64
  {
    public long hash(long arg,long aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    { // 15/50 to 19/50 avalanche
      arg = (~arg) ^ (arg >>> a2);  // 2
      arg = (arg + (arg << 2)) + (arg << a3);   // 2
      arg = arg ^ (arg >>> a4);  // 2
      arg = (arg) + (arg << a5); // 1 or 2
      arg = arg ^ (arg >>> a6);  // 2
      arg = (arg + (arg << 3)) + (arg << a7);  // 2
      arg = arg ^ (arg >>> a8);  // 2
      return arg; // 14 to 13
    }
  }
  static class hardalgo11 implements hashable64
  {
    public long hash(long arg,long aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    { // 22/50
      arg = (~arg) ^ (arg >>> 25);  // 2
      arg = (arg + (arg << 1) ) + (arg << 28);   // 2
      arg = arg ^ (arg >>> 7);  // 2
      arg = (arg) + (arg << 16); // 2
      arg = arg ^ (arg >>> 4);  // 2
      arg = (arg + (arg << 2)) + (arg << 14);  // 2
      arg = arg ^ (arg >>> 32);  // 2
      return arg; // 14
    }
  }
  static class algo12 implements hashable64
  {
    public long hash(long key,long aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      long c1=aux;
      long c2=0x1d243c4a448d5f6dL;
      key = (c1 ^ key) ^ (key >>> 32);
      key = key * c2;
      key = (key) ^ (key >>> 31);
      key = key * c1;
      key = key ^ ((key) >>> 32);
      return key;
    }
  }
  static class algo13 implements hashable64
  {
    public long hash(long key,long aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      long c2=aux;
      key = (key) ^ (key >>> a2);
      key = (~key) + (key << a3);
      key = (key) ^ (key >>> a4);
      key = key * c2;
      key = key ^ ((key) >>> a8);
      return key;
    }
  }
  static class hardalgo12 implements hashable64
  {
    public long hash(long key,long aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      long c1=0x43bd1adfd027f809L;
      long c2=0x1d243c4a448d5f6dL;
      key = (c1 ^ key) ^ (key >>> 32);
      key = key * c2;
      key = (key) ^ (key >>> 31);
      key = key * c1;
      key = key ^ ((key) >>> 32);
      return key;
    }
  }
  static class hardalgo13 implements hashable64
  {
    public long hash(long key,long aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      long c2=aux;
      key = (key) ^ (key >>> 32);
      key = (~key) + (key << 13);
      key = (key) ^ (key >>> 10);
      key = key * c2;
      key = key ^ ((key) >>> 31);
      return key;
    }
  }
  static class algo16a implements hashable6432
  {
    public int hash(long arg,long aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      arg = (arg << a2) - arg - 1;   // 2
      arg = arg ^ (arg >>> a3);  // 2
      arg = (arg   + (arg << 3)  ) + (arg << a4);  // 2
      arg = arg ^ (arg >>> a6);  // 2
      arg = (arg  /* + (arg << 3) */ ) + (arg << a7);
      arg = arg ^ (arg >>> a8);
      return (int) arg; // 12
    }
  }
  static class algo16b implements hashable6432
  {
    public int hash(long arg,long aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      arg = arg - (arg << a2) - 1;   // 2
      arg = arg ^ (arg >>> a3);  // 2
      arg = (arg   + (arg << 3)  ) + (arg << a4);  // 2
      arg = arg ^ (arg >>> a6);  // 2
      arg = (arg  /* + (arg << 3) */ ) + (arg << a7);
      arg = arg ^ (arg >>> a8);
      return (int) arg; // 12
    }
  }
  static class algo16 implements hashable6432
  {
    public int hash(long arg,long aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      arg = (arg << a2) - arg - 1;   // 2
      arg = arg ^ (arg >>> a3);  // 2
      arg = (arg   + (arg << 2)  ) + (arg << a4);  // 2
      arg = arg ^ (arg >>> a6);  // 2
      arg = (arg  /* + (arg << 3) */ ) + (arg << a7);
      arg = arg ^ (arg >>> a8);
      return (int) arg; // 12
    }
  }
  static class algo17 implements hashable6432
  {
    public int hash(long arg,long aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      arg = (arg) - (arg << a2) - 1;   // 2
      arg = arg ^ (arg >>> a3);  // 2
      arg = (arg   + (arg << 2)  ) + (arg << a4);  // 2
      arg = arg ^ (arg >>> a6);  // 2
      arg = (arg /* + (arg << 3) */) + (arg << a7);
      arg = arg ^ (arg >>> a8);
      return (int) arg; // 12
    }
  }
  static class algo18 implements hashable6432
  {
    public int hash(long arg,long aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      arg = (arg -1) - (arg << a2);   // 2
      arg = arg ^ (arg >>> a3);  // 2
      arg = (arg  + (arg << 2)  ) + (arg << a4);  // 2
      arg = arg ^ (arg >>> a6);  // 2
      arg = (arg + (arg << 3) ) + (arg << a7);
      arg = arg ^ (arg >>> a8);
      return (int) arg; // 12
    }
  }
  static class algo19 implements hashable6432
  {
    public int hash(long arg,long aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      arg = (arg << a2) - arg - 1;   // 2
      arg = arg ^ (arg >>> a3);  // 2
      arg = (arg  + (arg << 2)  ) + (arg << a4);  // 2
      arg = arg ^ (arg >>> a6);  // 2
      arg = (arg + (arg << 3) ) + (arg << a7);
      arg = arg ^ (arg >>> a8);
      return (int) arg; // 12
    }
  }
  static class hardalgo19 implements hashable6432
  {
    public int hash(long arg,long aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      arg = (~arg) + (arg << 31);   // 2
      arg = arg ^ (arg >>> 28);  // 2
      arg = /* ( arg  + (arg << 2) ) + (arg << 6); */ arg * (1+4+(1<<6)); // 2 or 1
      arg = arg ^ (arg >>> 13);  // 2
      arg = /* (arg + (arg << 3)) + (arg << 5);*/ arg * (1+8+(1<<5)); // 2 or 1
      arg = arg ^ (arg >>> 24);  // 2
      return (int) arg; // 12 to 10
    }
  }
  static class algo20 implements hashable6432
  {
    public int hash(long arg,long aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      arg = arg ^ (arg >>> a2);  // 2
      arg = (~arg) + (arg << a3);  // 2
      arg = arg ^ (arg >>> a4);  // 2
      arg = (arg /* + (arg << 2) */ ) + (arg << a5);   // 2 or 1
      arg = arg ^ (arg >>> a6);  // 2
      arg = (arg /* + (arg << 3) */) + (arg << a7);  // 2 or 1
      arg = arg ^ (arg >>> a8);  // 2
      return (int) arg; // 14 to 13
    }
  }
  static class hardalgo20 implements hashable6432
  {
    public int hash(long arg,long aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    { // 46/50 avalanche
      arg = arg ^ (arg >>> 32);  // 2
      arg = (~arg) + (arg << 30);  // 2
      arg = arg ^ (arg >>> 11);  // 2
      arg = (arg /* + (arg << 2) */) + (arg << 2);   // 2 or 1
      arg = arg ^ (arg >>> 15);  // 2
      arg = (arg /* + (arg << 3) */) + (arg << 8);  // 2 or 1
      arg = arg ^ (arg >>> 32);  // 2
      return (int) arg; // 14 to 12
    }
  }
  static class algo21 implements hashable6432 // 
  {
    public int hash(long arg,long aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      arg = (~arg) + (arg << a2);   // 2 or 1
      arg = arg ^ (arg >>> a3);  // 2
      arg = (arg) + (arg << a4);  // 2
      arg = arg ^ (arg >>> a5);  // 2
      arg = arg + (arg << a6);
      arg = arg ^ (arg >>> a7);  // 2
      arg = (arg << a8) - arg;
      return (int) arg; // 12 or 11
    }
  }
  static class hardalgo21 implements hashable6432
  {
    public int hash(long arg,long aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    { // 46/50 avalanche
      arg = (~arg) + (arg << 31);   // 2
      arg = arg ^ (arg >>> 9);  // 2
      arg = (arg) + (arg << 2); // 2 or 1
      arg = arg ^ (arg >>> 17);  // 2
      arg = arg + (arg << 3);   // 2 or 1
      arg = arg ^ (arg >>> 21);   // 2
      arg = arg + (arg << 29);   // 2
      return (int) arg; // 14 to 12
    }
  }
  static class algo22 implements hashable6432
  {
    public int hash(long key,long aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      long c2=aux;
      key = key ^ (key >>> a2);
      key = (~key) + (key << a3);
      key = key ^ (key >>> a4);
      key = key * c2;
      key = key ^ ((key) >>> a8);
      return (int) key;
    }
  }
  static class hardalgo22 implements hashable6432
  {
    public int hash(long key,long aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      long c2=0xbf9df6392d13b2d5L;
      key = key ^ (key >>> 32);
      key = (~key) + (key << 15);    
      key = key ^ (key >>> 12);
      key = key * c2;
      key = key ^ ((key) >>> 31);
      return (int) key;
    }
  }
  static class algo23 implements hashable6432
  {
    public int hash(long key,long aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      long c1=aux;
      long c2=0x1d243c4a448d5f6dL;
      key = (c1 ^ key) ^ (key >>> 32);
      key = key * c2;
      key = (c2 ^ key) ^ (key >>> 31);
      key = key * c1;
      key = key ^ (key >>> 32);
      return (int) key;
    }
  }
  static class hardalgo23 implements hashable6432
  {
    public int hash(long key,long aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      long c1=0x43bd1adfd027f809L;
      long c2=0x1d243c4a448d5f6dL;
      key = (c1 ^ key) ^ (key >>> 32);
      key = key * c2;
      key = (c2 ^ key) ^ (key >>> 31);
      key = key * c1;
      key = key ^ ((key) >>> 32);
      return (int) key;
    }
  }
  
  static class algo0 implements hashable32 // 6 rounds
  {
    public int hash(int arg,int aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      if ((a2 & 4) == 0)
        arg = (arg - 1) - (arg << a3);  // 2
      else
        arg = (arg << a3) - arg - 1;
      arg = (arg) ^ (arg >>> a4);  // 2
      arg = (arg) + (arg << a5);   // 2 or 1
      arg = (arg) ^ (arg >>> a6);  // 2
      switch (a2 & 3) {
      case 0:
        arg = (arg  + (arg << 3)) + (arg << a7);  // 2
        break;
      case 1:
        arg = (arg  + (arg << 2)) + (arg << a7);  // 2
        break;
      case 2:
        arg = (arg  + (arg << 3)) - (arg << a7);  // 2
        break;
      case 3:
        arg = (arg  + (arg << 2)) - (arg << a7);  // 2
        break;
      }
      arg = (arg) ^ (arg >>> a8);  // 2
      return arg; // 12 or 11
    }
  }
  static class hardalgo0 implements hashable32
  {
    public int hash(int arg,int aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      arg = (arg + 0x5b3d) + (arg << 16);  // 2
      arg = (arg ^ 73) ^ (arg >>> 15);  // 2
      arg = arg + (arg << 3);   // 2 or 1
      arg = (arg ^ 91) ^ (arg >>> 2);  // 2
      arg = arg * ((1<<10) + 9);
      // arg = (arg + (arg << 3)) + (arg << 13);  // 2
      arg = (arg ^ 59) ^ (arg >>> 15);  // 2
      return arg; // 12 or 11
    }
  }
  static class hardalgo00 implements hashable32
  {
    public int hash(int arg,int aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      arg += ~(arg << 15);  // 2
      arg = (arg) ^ (arg >>> 10);  // 2
      arg = arg * 9; // arg + (arg << 3);   // 2 or 1
      arg = arg ^ (arg >>> 6);
      arg +=  ~(arg << 11);  // 2
      arg = (arg) ^(arg >>> 16);  // 2
      return arg; // 12 or 11
    }
  }

  static class algo1 implements hashable32
  {
    public int hash(int arg,int aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      arg = (arg << a3) - arg - 1;  // 2
      arg = arg ^ (arg >>> a4);  // 2
      arg = (arg) + (arg << a5);   // 2 or 1
      arg = (arg) ^ (arg >>> a6);  // 2
      arg = (arg) + (arg << a7);  // 2
      arg = arg ^ (arg >>> a8);  // 2
      return arg; // 12 or 11
    }
  }
  static class hardalgo_a implements hashable32
  {
    public int hash(int arg,int aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      arg = arg - (arg << 15) - 1;  // 2
      arg = arg ^ (arg >>> 13);  // 2
      // arg = (arg) + (arg << 2);   // 2 or 1
      arg = arg * 5;
      arg = (arg) ^ (arg >>> 4);  // 2
      // arg = (arg  + (arg << 3) ) + (arg << 11);  // 2
      arg = (arg) * (1 + (1 << 3) + (1 << 11));
      arg = arg ^ (arg >>> 16);  // 2
      return arg; // 12 or 11
    }
  }
  static class algo_a implements hashable32
  {
    public int hash(int arg,int aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      arg = arg - (arg << a3) - 1;  // 2
      arg = arg ^ (arg >>> a4);  // 2
      arg = (arg /* + (arg << 2) */) + (arg << a5);   // 2 or 1
      arg = (arg) ^ (arg >>> a6);  // 2
      arg = (arg  + (arg << 3) ) + (arg << a7);  // 2
      arg = arg ^ (arg >>> a8);  // 2
      return arg; // 12 or 11
    }
  }
  static class algo_b implements hashable32
  {
    public int hash(int arg,int aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      arg = (arg << a3) - arg - 1;  // 2
      arg = arg ^ (arg >>> a4);  // 2
      arg = (arg /* + (arg << 2) */) + (arg << a5);   // 2 or 1
      arg = (arg) ^ (arg >>> a6);  // 2
      arg = (arg + (arg << 3)) + (arg << a7);  // 2
      arg = arg ^ (arg >>> a8);  // 2
      return arg; // 12 or 11
    }
  }
  static class algo_c implements hashable32
  {
    public int hash(int arg,int aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      arg = arg - (arg << a3) - 1;  // 2
      arg = arg ^ (arg >>> a4);  // 2
      arg = (arg /* + (arg << 2) */) + (arg << a5);   // 2 or 1
      arg = (arg) ^ (arg >>> a6);  // 2
      arg = (arg  + (arg << 2) ) + (arg << a7);  // 2
      arg = arg ^ (arg >>> a8);  // 2
      return arg; // 12 or 11
    }
  }
  static class algo_d implements hashable32
  {
    public int hash(int arg,int aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      arg = (arg << a3) - arg - 1;  // 2
      arg = arg ^ (arg >>> a4);  // 2
      arg = (arg /* + (arg << 2) */) + (arg << a5);   // 2 or 1
      arg = (arg) ^ (arg >>> a6);  // 2
      arg = (arg  + (arg << 2) ) + (arg << a7);  // 2
      arg = arg ^ (arg >>> a8);  // 2
      return arg; // 12 or 11
    }
  }
  static class algo_e implements hashable32
  {
    public int hash(int arg,int aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      arg = arg - (arg << a3) - 1;  // 2
      arg = arg ^ (arg >>> a4);  // 2
      arg = (arg + (arg << 2)) + (arg << a5);   // 2 or 1
      arg = (arg) ^ (arg >>> a6);  // 2
      arg = (arg  /* + (arg << 3) */) + (arg << a7);  // 2
      arg = arg ^ (arg >>> a8);  // 2
      return arg; // 12 or 11
    }
  }
  static class algo_f implements hashable32
  {
    public int hash(int arg,int aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      arg = (arg << a3) - arg - 1;  // 2
      arg = arg ^ (arg >>> a4);  // 2
      arg = (arg  + (arg << 2) ) + (arg << a5);   // 2 or 1
      arg = (arg) ^ (arg >>> a6);  // 2
      arg = (arg /*+ (arg << 3) */) + (arg << a7);  // 2
      arg = arg ^ (arg >>> a8);  // 2
      return arg; // 12 or 11
    }
  }
  static class algo_g implements hashable32
  {
    public int hash(int arg,int aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      arg = arg - (arg << a3) - 1;  // 2
      arg = arg ^ (arg >>> a4);  // 2
      arg = (arg  + (arg << 3) ) + (arg << a5);   // 2 or 1
      arg = (arg) ^ (arg >>> a6);  // 2
      arg = (arg /* + (arg << 2) */) + (arg << a7);  // 2
      arg = arg ^ (arg >>> a8);  // 2
      return arg; // 12 or 11
    }
  }
  static class algo_h implements hashable32
  {
    public int hash(int arg,int aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      arg = (arg << a3) - arg - 1;  // 2
      arg = arg ^ (arg >>> a4);  // 2
      arg = (arg  + (arg << 3) ) + (arg << a5);   // 2 or 1
      arg = (arg) ^ (arg >>> a6);  // 2
      arg = (arg /* + (arg << 2) */) + (arg << a7);  // 2
      arg = arg ^ (arg >>> a8);  // 2
      return arg; // 12 or 11
    }
  }
  static class hardalgo1 implements hashable32
  {
    public int hash(int arg,int aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      arg = (arg << 13) - arg - 1;  // 2
      arg = arg ^ (arg >>> 10);  // 2
      arg = arg + (arg << 3);   // 2 or 1
      arg = (arg) ^ (arg >>> 6);  // 2
      arg = arg * ((1<<14) + 5);
      // arg = (arg + (arg << 2)) + (arg << 14);  // 2
      arg = arg ^ (arg >>> 16);  // 2
      return arg; // 12 or 11
    }
  }
  static class algo2 implements hashable32
  {
    public int hash(int key,int aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      int c1=aux;
      int c2=0x4192fa13;
      key = (c1 ^ key) ^ (key >>> 16);
      key = key * c2;
      key = (key) ^ (key >>> 15);
      key = key * c1;
      key = key ^ ((key) >>> 16);
      return key;
    }
  }
  static class algo3 implements hashable32
  {
    public int hash(int key,int aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      int c2=aux;
      key = (key ^ a2) ^ (key >>> a3);
      key = key + (key << a5);
      key = (key) ^ (key >>> a6);
      key = key * c2;
      key = key ^ ((key) >>> a8);
      return key;
    }
  }
  static class hardalgo3 implements hashable32
  {
    public int hash(int key,int aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      int c2=0x9c6c8f5b;
      key = (key ^ 43) ^ (key >>> 15);
      key = (key) + (key << 6);
      key = (key) ^ (key >>> 4);
      key = key * c2;
      key = key ^ ((key) >>> 16);
      return key;
    }
  }
  static class algo4 implements hashable32
  {
    public int hash(int arg,int aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      arg = (~arg) + (arg << a3);  // 2
      arg = arg ^ (arg >>> a4);  // 2
      arg = arg + (arg << a5);   // 2 or 1
      arg = arg ^ (arg >>> a6);  // 2
      arg = (arg) + ~(arg << a7);  // 2
      arg = arg ^ (arg >>> a8);  // 2
      return arg; // 12 or 11
    }
  }
  static class algo5 implements hashable32
  {
    public int hash(int arg,int aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      arg = (~arg) + (arg << a3);   // 2 or 1
      arg = arg ^ (arg >>> a4);  // 2
      arg = (arg) + (arg << a5);   // 2 or 1
      arg = (arg) ^ (arg >>> a6); // 2
      arg = (arg) + (arg << a7);  // 2
      arg = (arg | 64) ^ ((arg << (32-a8)) | (arg >>> a8));  // 2
      return arg; // 12 or 11
    }
  }
  static class hardalgo5 implements hashable32
  {
    public int hash(int arg,int aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      arg = (~arg) + (arg << 16);   // 2 or 1
      arg = arg ^ (arg >>> 13);  // 2
      arg = (arg) + (arg << 3);   // 2 or 1
      arg = (arg) ^ (arg >>> 8); // 2
      arg = (arg) + (arg << 2);  // 2
      arg = (arg | 64) ^ ((arg << (15)) | (arg >>> 17));  // 2
      return arg; // 12 or 11
    }
  }
  static class algo6 implements hashable32
  {
    public int hash(int arg,int aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      arg = (arg) + ~(arg << a3);   // 2 or 1
      arg = arg ^ (arg >>> a4);  // 2
      arg = (arg) + (arg << a5);   // 2 or 1
      arg = (arg) ^ (arg >>> a6); // 2
      arg = (arg) + (arg << a7);  // 2
      arg = (arg | 64) ^ ((arg << (32-a8)) ^ (arg >>> a8));  // 2
      return arg; // 12 or 11
    }
  }
  static class algo7 implements hashable32 // 7 rounds
  {
    public int hash(int arg,int aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      arg = (arg) - (arg << a2) - 1;   // 2
      arg = arg ^ (arg >>> a3);  // 2
      arg = (arg) + (arg << a4);   // 2 or 1
      arg = (arg) ^ (arg >>> a5); // 2
      arg = (arg) + (arg << a6);  // 2 or 1
      arg = (arg) ^ (arg >>> a7);  // 2
      arg = (arg) + (arg << a8);  // 2
      return arg; // 14 or 12
    }
  }
  
  static class hardalgo2 implements hashable32
  {
    public int hash(int key,int aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      int c1=0xda6217ad;
      int c2=0x4192fa13;
      key = (c1 ^ key) ^ (key >>> 16);
      key = key * c2;
      key = (key) ^ (key >>> 15);
      key = key * c1;
      key = key ^ ((key) >>> 16);
      return key;
    }
  }
  static class hardalgo4 implements hashable32
  {
    public int hash(int arg,int aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      arg = (arg) + ~(arg << 15);  // 2
      arg = arg ^ (arg >>> 10);  // 2
      arg = arg + (arg << 3);   // 2 or 1
      arg = arg ^ (arg >>> 6);  // 2
      arg = (arg) + ~(arg << 11);  // 2
      arg = arg ^ (arg >>> 16);  // 2
      return arg; // 12 or 11
    }
  }
  static class hardalgo7 implements hashable32
  {
    public int hash(int arg,int aux,int a2,int a3,int a4,
      int a5,int a6,int a7,int a8)
    {
      arg = (arg) + ~(arg << 13);  // 2
      arg = arg ^ (arg >>> 15);  // 2
      arg = arg + (arg << 2);   // 2 or 1
      arg = arg ^ (arg >>> 7);  // 2
      arg = (arg) + (arg << 3);  // 2 or 1
      arg = arg ^ (arg >>> 13);  // 2
      arg = arg + (arg << 12);   // 2
      return arg; // 12 or 11
    }
  }
  static final int numtrials32=16;
  static boolean tryit32(hashable32 algo, int aux, int a2,
    int a3,int a4,int a5,int a6,int a7,int a8)
  {
    int changed, nochange, result1, result2, bitp;
    int trials;

    changed = nochange = -1;
    bitp = 1 << numtrials32;
    for (trials = numtrials32; --trials >= 0; )
    {
      bitp = bitp >>> 1;
      result1 = algo.hash(bitp, aux, a2,a3,a4,a5,a6,a7,a8);
      changed &= result1;
      nochange &= ~result1;
      if ((changed | nochange) == 0) break;
    }
    if (trials < 0) return false;

    changed = nochange = -1;
    bitp = 1 << 31;
    for (trials = numtrials32; --trials >= 0; )
    {
      bitp = bitp >>> 1;
      result1 = algo.hash(bitp, aux, a2,a3,a4,a5,a6,a7,a8);
      changed &= result1;
      nochange &= ~result1;
      if ((changed | nochange) == 0) break;
    }
    if (trials < 0) return false;

    changed = nochange = -1;
    bitp = 0;
    for (trials = numtrials32; --trials >= 0; )
    {
      result1 = algo.hash(bitp, aux, a2,a3,a4,a5,a6,a7,a8);
      bitp = bitp + 10;
      changed &= result1;
      nochange &= ~result1;
      if ((changed | nochange) == 0) break;
    }
    if (trials < 0) return false;

    changed = nochange = -1;
    bitp = 0;
    for (trials = numtrials32; --trials >= 0; )
    {
      result1 = algo.hash(bitp, aux, a2,a3,a4,a5,a6,a7,a8);
      bitp = bitp + 100;
      changed &= result1;
      nochange &= ~result1;
      if ((changed | nochange) == 0) break;
    }
    if (trials < 0) return false;

    for (trials = 32 - 4; trials >= 0; trials -= 4)
    {
      changed = nochange = -1;
      for (bitp = numtrials32; bitp > 0; --bitp)
      {
        int keyarg = (bitp << trials) ;
        result1 = algo.hash(keyarg,aux,a2,a3,a4,a5,a6,a7,a8);
        changed &= result1;
        nochange &= ~result1;
        if ((changed | nochange) == 0L) break;
      }
      if (bitp < 1) return false;
    }

    for (bitp=1 << 31; bitp != 0; bitp = bitp >>> 1)
    {
      changed = nochange = -1;
      for (trials = numtrials32; --trials >= 0; )
      {
        int keyarg = nextInt() & nextInt();
        result1 = algo.hash(keyarg,aux,a2,a3,a4,a5,a6,a7,a8);
        result2 = algo.hash(keyarg ^ bitp,aux,a2,a3,a4,a5,a6,a7,a8);
        changed &= ~(result1 ^ result2);
        nochange &= result1 ^ result2;
        if ((changed | nochange) == 0L) break;
      }
      if (trials < 0) return false;
    }
    for (bitp=1 << 31; bitp != 0; bitp = bitp >>> 1)
    {
      changed = nochange = -1;
      for (trials = numtrials32; --trials >= 0; )
      {
        int keyarg = nextInt() & nextInt();
        result1 = algo.hash(keyarg,aux,a2,a3,a4,a5,a6,a7,a8);
        result2 = algo.hash(keyarg + bitp,aux,a2,a3,a4,a5,a6,a7,a8);
        changed &= ~(result1 ^ result2);
        nochange &= result1 ^ result2;
        if ((changed | nochange) == 0L) break;
      }
      if (trials < 0) return false;
    }

    return true;
  }

  static final int numtrials64=18;
  static boolean tryit64(hashable64 algo, long aux, int a2,
    int a3,int a4,int a5,int a6,int a7,int a8)
  {
    long changed, nochange, result1, result2, bitp;
    int trials;

    changed = nochange = -1L;
    bitp = 1L << numtrials64;
    for (trials = numtrials64; --trials >= 0; )
    {
      bitp = bitp >>> 1;
      result1 = algo.hash(bitp, aux, a2,a3,a4,a5,a6,a7,a8);
      changed &= result1;
      nochange &= ~result1;
      if ((changed | nochange) == 0L) break;
    }
    if (trials < 0) return false;

    changed = nochange = -1L;
    bitp = 1L << 63;
    for (trials = numtrials64; --trials >= 0; )
    {
      bitp = bitp >>> 1;
      result1 = algo.hash(bitp, aux, a2,a3,a4,a5,a6,a7,a8);
      changed &= result1;
      nochange &= ~result1;
      if ((changed | nochange) == 0L) break;
    }
    if (trials < 0) return false;

    changed = nochange = -1L;
    bitp = 0L;
    for (trials = numtrials64; --trials >= 0; )
    {
      result1 = algo.hash(bitp ,aux, a2,a3,a4,a5,a6,a7,a8);
      bitp = bitp + 100L;
      changed &= result1;
      nochange &= ~result1;
      if ((changed | nochange) == 0L) break;
    }
    if (trials < 0) return false;

    changed = nochange = -1L;
    bitp = 0L;
    for (trials = numtrials64; --trials >= 0; )
    {
      result1 = algo.hash(bitp, aux, a2,a3,a4,a5,a6,a7,a8);
      bitp = bitp + 10L;
      changed &= result1;
      nochange &= ~result1;
      if ((changed | nochange) == 0L) break;
    }
    if (trials < 0) return false;

    for (trials=64 - 1; trials >= 0; trials -= 1)
    {
      changed = nochange = -1;
      for (bitp = numtrials64; bitp > 0; --bitp)
      {
        long keyarg = (bitp << trials) | (bitp >>> (64-trials));
        result1 = algo.hash(keyarg,aux,a2,a3,a4,a5,a6,a7,a8);
        changed &= result1;
        nochange &= ~result1;
        if ((changed | nochange) == 0L) break;
      }
      if (bitp < 1) return false;
    }

    for (bitp=1L << 63; bitp != 0L; bitp = bitp >>> 1)
    {
      changed = nochange = -1L;
      for (trials = numtrials64; --trials >= 0; )
      {
        long keyarg = nextLong() & nextLong() & nextLong();
        result1 = algo.hash(keyarg,aux,a2,a3,a4,a5,a6,a7,a8);
        result2 = algo.hash(keyarg + bitp,aux,a2,a3,a4,a5,a6,a7,a8);
        changed &= ~(result1 ^ result2);
        nochange &= result1 ^ result2;
        if ((changed | nochange) == 0L) break;
      }
      if (trials < 0) return false;
    }
    for (bitp=1L << 63; bitp != 0L; bitp = bitp >>> 1)
    {
      changed = nochange = -1L;
      for (trials = numtrials64; --trials >= 0; )
      {
        long keyarg = nextLong() & nextLong() & nextLong();
        result1 = algo.hash(keyarg,aux,a2,a3,a4,a5,a6,a7,a8);
        result2 = algo.hash(keyarg ^ bitp,aux,a2,a3,a4,a5,a6,a7,a8);
        changed &= ~(result1 ^ result2);
        nochange &= result1 ^ result2;
        if ((changed | nochange) == 0L) break;
      }
      if (trials < 0) return false;
    }
    return true;
  }
  static boolean boxit64(hashable64 algo, int op, float max_diff,
   long aux, int a2, int a3, int a4,
   int a5, int a6, int a7, int a8)
  {
   final int total_trials = 300;
   final int max_threshold = (int) ((0.5f + max_diff) * total_trials);
   final int min_threshold = (int) ((0.5f - max_diff) * total_trials);
   int r, c;
   long result1, result2;
   long inp, outp;
   clearRecBox(64,64);
   for (r = 64; --r >= 0; )
   {
     inp = 1L << r;
     for (int trials = total_trials; --trials >= 0; )
     {
       long keyarg = nextLong() & nextLong() & nextLong();
       long keyarg2 = (op == 1) ? (keyarg + inp) : (keyarg ^ inp);
       result1 = algo.hash(keyarg,aux,a2,a3,a4,a5,a6,a7,a8);
       result2 = algo.hash(keyarg2,aux,a2,a3,a4,a5,a6,a7,a8);
       for (c = 64; --c >= 0; )
       {
         outp = 1L << c;
         if (((result1 ^ result2) & outp) != 0L)
           bumpUpRecBox(r, c, 64);
       }
     }  
     int min = getMinEntry(r,64);
     int max = getMaxEntry(r,64);
     if ((min < min_threshold) || (max > max_threshold)) return false;
   }
   return true;
 }

  static final int numtrials6432=17;
  static boolean boxit6432(hashable6432 algo, int op, float max_diff,
   long aux, int a2, int a3, int a4,
   int a5, int a6, int a7, int a8)
  {
   final int total_trials = 200;
   final int max_threshold = (int) ((0.5f + max_diff) * total_trials);
   final int min_threshold = (int) ((0.5f - max_diff) * total_trials);
   int r, c;
   int result1, result2;
   long inp;
   int outp;
   clearRecBox(64,32);
   for (r = 64; --r >= 0; )
   {
     inp = 1L << r;
     for (int trials = total_trials; --trials >= 0; )
     {
       long keyarg = nextLong() & nextLong() & nextLong();
       long keyarg2 = (op == 1) ? (keyarg + inp) : (keyarg ^ inp);
       result1 = algo.hash(keyarg,aux,a2,a3,a4,a5,a6,a7,a8);
       result2 = algo.hash(keyarg2,aux,a2,a3,a4,a5,a6,a7,a8);
       for (c = 32; --c >= 0; )
       {
         outp = 1 << c;
         if (((result1 ^ result2) & outp) != 0)
           bumpUpRecBox(r, c, 32);
       }
     }  
     int min = getMinEntry(r,32);
     int max = getMaxEntry(r,32);
     if ((min < min_threshold) || (max > max_threshold)) return false;
   }
   return true;
 }
 static boolean tryit6432(hashable6432 algo, long aux, int a2,
    int a3,int a4,int a5,int a6,int a7,int a8)
  {
    int changed, nochange, result1, result2;
    long bitp;
    int trials;
    changed = nochange = -1;
    bitp = 1L << numtrials6432;
    for (trials = numtrials6432; --trials >= 0; )
    {
      bitp = bitp >>> 1;
      result1 = algo.hash(bitp, aux, a2,a3,a4,a5,a6,a7,a8);
      changed &= result1;
      nochange &= ~result1;
      if ((changed | nochange) == 0) break;
    }
    if (trials < 0) return false;

    changed = nochange = -1;
    bitp = 1L << 63;
    for (trials = numtrials6432; --trials >= 0; )
    {
      bitp = bitp >>> 1;
      result1 = algo.hash(bitp, aux, a2,a3,a4,a5,a6,a7,a8);
      changed &= result1;
      nochange &= ~result1;
      if ((changed | nochange) == 0) break;
    }
    if (trials < 0) return false;

    changed = nochange = -1;
    bitp = 0;
    for (trials = numtrials6432; --trials >= 0; )
    {
      result1 = algo.hash(bitp, aux, a2,a3,a4,a5,a6,a7,a8);
      bitp = bitp + 100L;
      changed &= result1;
      nochange &= ~result1;
      if ((changed | nochange) == 0) break;
    }
    if (trials < 0) return false;

    changed = nochange = -1;
    bitp = 0;
    for (trials = numtrials6432; --trials >= 0; )
    {
      result1 = algo.hash(bitp, aux, a2,a3,a4,a5,a6,a7,a8);
      bitp = bitp + 10L;
      changed &= result1;
      nochange &= ~result1;
      if ((changed | nochange) == 0) break;
    }
    if (trials < 0) return false;

    for (trials=64 - 2; trials >= 0; trials -= 2)
    {
      changed = nochange = -1;
      for (bitp = numtrials6432; bitp > 0; --bitp)
      {
        long keyarg = (bitp << trials) | (bitp >>> (64-trials));
        result1 = algo.hash(keyarg,aux,a2,a3,a4,a5,a6,a7,a8);
        changed &= result1;
        nochange &= ~result1;
        if ((changed | nochange) == 0L) break;
      }
      if (bitp < 1) return false;
    }

    for (bitp=1L << 63; bitp != 0L; bitp = bitp >>> 1)
    {
      changed = nochange = -1;
      for (trials = numtrials6432; --trials >= 0; )
      {
        long keyarg = nextLong() & nextLong() & nextLong();
        result1 = algo.hash(keyarg,aux,a2,a3,a4,a5,a6,a7,a8);
        result2 = algo.hash(keyarg + bitp,aux,a2,a3,a4,a5,a6,a7,a8);
        changed &= ~(result1 ^ result2);
        nochange &= result1 ^ result2;
        if ((changed | nochange) == 0) break;
      }
      if (trials < 0) return false;
    }

    for (bitp=1L << 63; bitp != 0L; bitp = bitp >>> 1)
    {
      changed = nochange = -1;
      for (trials = numtrials6432; --trials >= 0; )
      {
        long keyarg = nextLong() & nextLong() & nextLong();
        result1 = algo.hash(keyarg,aux,a2,a3,a4,a5,a6,a7,a8);
        result2 = algo.hash(keyarg ^ bitp,aux,a2,a3,a4,a5,a6,a7,a8);
        changed &= ~(result1 ^ result2);
        nochange &= result1 ^ result2;
        if ((changed | nochange) == 0) break;
      }
      if (trials < 0) return false;
    }
    return true;
  }
 static boolean boxit32(hashable32 algo, int op, float max_diff,
   int aux, int a2, int a3, int a4,
   int a5, int a6, int a7, int a8)
 {
   final int total_trials = 200;
   final int max_threshold = (int) ((0.5f + max_diff) * total_trials);
   final int min_threshold = (int) ((0.5f - max_diff) * total_trials);
   int r, c;
   int result1, result2;
   int inp, outp;
   clearRecBox(32,32);
   for (r = 32; --r >= 0; )
   {
     inp = 1 << r;
     for (int trials = total_trials; --trials >= 0; )
     {
       int keyarg = nextInt() & nextInt();
       int keyarg2 = (op == 1) ? (keyarg + inp) : (keyarg ^ inp);
       result1 = algo.hash(keyarg,aux,a2,a3,a4,a5,a6,a7,a8);
       result2 = algo.hash(keyarg2, aux,a2,a3,a4,a5,a6,a7,a8);
       for (c = 32; --c >= 0; )
       {
         outp = 1 << c;
         if (((result1 ^ result2) & outp) != 0)
           bumpUpRecBox(r, c, 32);
       }
     }  
     int min = getMinEntry(r,32);
     int max = getMaxEntry(r,32);
     if ((min < min_threshold) || (max > max_threshold)) return false;
   }
   return true;
 }
 static void driver32(hashable32 myhash, float max_diff, int a2init,
 int a3init, int a4init, int a5init, int a6init, int a7init, int a8init)
{
 int a2, a3, a4, a5, a6, a7, a8;
 int good, bad, indx;
 for (a2=a2init; --a2 >= 0; )
 {
  for (a3=a3init; --a3 >= 3; )
  {
   for (a4=a4init; --a4 >= 2; )
   {
    for (a5=a5init; --a5 >= 1; )
    {
     for (a6=a6init; --a6 >= 1; )
     {
      for (a7=a7init; --a7 >= 2; )
      {
       for (a8=a8init; --a8 >= 13; )
       {
        int aux = nextInt() | 1;
        if (! tryit32(myhash,aux,a2,a3,a4,a5,a6,a7,a8)) continue;
        if (! boxit32(myhash,0,max_diff,aux,a2,a3,a4,a5,a6,a7,a8)) continue;
        if (! boxit32(myhash,1,max_diff,aux,a2,a3,a4,a5,a6,a7,a8)) continue;
        {
          // best = good;
          System.out.println(
          Integer.toHexString(aux)+
          ','+a2+','+a3+','+a4+','+a5+','+a6+','+a7+','+a8);
        }
       }
      }
     }
    }
   }
  }
 }
}
 static void driver64(hashable64 myhash, float max_diff, int a2init,
 int a3init, int a4init, int a5init, int a6init, int a7init, int a8init)
{
 int a2, a3, a4, a5, a6, a7, a8;
 int indx;
 for (a2=a2init; --a2 >= 19; )
 {
  for (a3=a3init; --a3 >= 18; )
  {
   for (a4=a4init; --a4 >= 4; )
   {
    for (a5=a5init; --a5 >= 3; )
    {
     for (a6=a6init; --a6 >= 4; )
     {
      for (a7=a7init; --a7 >= 18; )
      {
       for (a8=a8init; --a8 >= 26; )
       {
        long aux  =  nextLong() |  1;
        if (! tryit64(myhash,aux,a2,a3,a4,a5,a6,a7,a8)) continue;
        if (! boxit64(myhash,0,max_diff,aux,a2,a3,a4,a5,a6,a7,a8)) continue;
        if (! boxit64(myhash,1,max_diff,aux,a2,a3,a4,a5,a6,a7,a8)) continue;
        {
          // best = good;
          System.out.println(
          Long.toHexString(aux)+
          ','+a2+','+a3+','+a4+','+a5+','+a6+','+a7+','+a8);
        }
       }
      }
     }
    }
   }
  }
 }
}
 static void driver6432(hashable6432 myhash, float max_diff, int a2init,
 int a3init, int a4init, int a5init, int a6init, int a7init, int a8init)
{
 int a2, a3, a4, a5, a6, a7, a8;
 for (a2=a2init; --a2 >= 18; )
 {
  for (a3=a3init; --a3 >= 3; )
  {
   for (a4=a4init; --a4 >= 2; )
   {
    for (a5=a5init; --a5 >= 2; )
    {
     for (a6=a6init; --a6 >= 2; )
     {
      for (a7=a7init; --a7 >= 3; )
      {
       for (a8=a8init; --a8 >= 20; )
       {
        long aux  =  nextLong() |  1L;
        if (! tryit6432(myhash,aux,a2,a3,a4,a5,a6,a7,a8)) continue;
        if (! boxit6432(myhash,0,max_diff,aux,a2,a3,a4,a5,a6,a7,a8)) continue;
        if (! boxit6432(myhash,1,max_diff,aux,a2,a3,a4,a5,a6,a7,a8)) continue;
        {
          // best = good;
          System.out.println(
          Long.toHexString(aux)+
          ','+a2+','+a3+','+a4+','+a5+','+a6+','+a7+','+a8);
        }
       }
      }
     }
    }
   }
  }
 }
}
 static volatile int sink;
 
 public static long measure32(hashable32 myalgo, int loopcnt)
 {
   long t1 = System.currentTimeMillis();
   int tmp = sink;
   for (int indx = loopcnt; indx > 0; --indx)
   {
     tmp ^= myalgo.hash(indx, 0, 0,0,0,0,0,0,0);
   }
   sink += tmp;
   long t2 = System.currentTimeMillis();
   return t2 - t1;
 }
 public static long measure64(hashable64 myalgo, int loopcnt)
 {
   long t1 = System.currentTimeMillis();
   long tmp = (int) sink;
   for (int indx = loopcnt; indx > 0; --indx)
   {
     tmp ^= myalgo.hash((long) indx, 0L, 0,0,0,0,0,0,0);
   }
   sink += (int) tmp;
   long t2 = System.currentTimeMillis();
   return t2 - t1;
 } 
 public static long measure6432(hashable6432 myalgo, int loopcnt)
 {
   long t1 = System.currentTimeMillis();
   int tmp = sink;
   for (int indx = loopcnt; indx > 0; --indx)
   {
     tmp ^= myalgo.hash((long) indx, 0L, 0,0,0,0,0,0,0);
   }
   sink += tmp;
   long t2 = System.currentTimeMillis();
   return t2 - t1;
 }
 public static void emit32(hashable32 myalgo, String fname) throws IOException
 {
   PrintStream out = new PrintStream(new FileOutputStream(fname));
   for (int i = 0; i < 256; ++i)
   {
     int v = myalgo.hash(i, 0, 0,0,0,0,0,0,0);
     out.println(Integer.toHexString(v));
   }
   out.close();
 }
 public static void emit64(hashable64 myalgo, String fname) throws IOException
 {
   PrintStream out = new PrintStream(new FileOutputStream(fname));
   for (int i = 0; i < 256; ++i)
   {
     long v = myalgo.hash((long) i, 0L, 0,0,0,0,0,0,0);
     out.println(Long.toHexString(v));
   }
   out.close();
 }
public static void emit6432(hashable6432 myalgo, String fname) throws IOException
 {
   PrintStream out = new PrintStream(new FileOutputStream(fname));
   for (int i = 0; i < 256; ++i)
   {
     int v = myalgo.hash((long) i, 0L, 0,0,0,0,0,0,0);
     out.println(Integer.toHexString(v));
   }
   out.close();
 }
 public static void main(String[] argv) throws IOException
 {
  long t=0;
  int indx;
  // change/no-change = 2 trials
  // 32/64 xor trials, 5 sanity trials
  // 32/64 output channels
  double successrate64 =
  Math.pow(1.0d - Math.pow(0.5d, numtrials64) * 2.0, (64*2 + 32 + 4) * 64.0);
  System.out.println("successrate64 = " + successrate64);
  double successrate6432 =
  Math.pow(1.0d - Math.pow(0.5d, numtrials6432) * 2.0, (64*2 + 32 + 4) * 32.0);
  System.out.println("successrate6432 = " + successrate6432);
  double successrate32 =
  Math.pow(1.0d - Math.pow(0.5d, numtrials32) * 2.0, (32*2 + 8 + 4) * 32.0);
  System.out.println("successrate32 = " + successrate32);
  for(int i = 0; i < 13579; ++i) sink += nextInt();
/*
  System.out.println("64->32 algo16a");
  driver6432(new algo16a(), .185f, 33,32,18,3,24,28,33);
  System.out.println("64->32 algo16b");
  driver6432(new algo16b(), .185f, 33,32,18,3,24,28,33);
  System.out.println("64->32 algo16");
  driver6432(new algo16(), .185f, 33,32,18,3,24,28,33);
  System.out.println("64->32 algo17");
  driver6432(new algo17(), .185f, 33,32,18,3,24,28,33);
  System.out.println("64->32 algo18 shift 0");
  // driver6432(new algo18(), .17f, 33,32,18,3,24,28,33);
  System.out.println("64->32 algo19 shift 1");
  // driver6432(new algo19(), .18f, 33,32,18,3,24,28,33);
  System.out.println("64->32 algo20 shift 2");
  //  driver6432(new algo20(), .19f, 33,32,18,5,24,5,33);
  System.out.println("64->32 algo21 shift 3");
  // driver6432(new algo21(), .19f, 33,32,28,28,28,28,33);
  System.out.println("64->32 algorithm mult 1");
  // driver6432(new algo22(), .17f, 33,28,21,4,21,28,33);
  System.out.println("64->32 algorithm mult 2");
  // driver6432(new algo23(), .17f, 33,28,21,4,21,28,33);

  hashable6432 hardalgo6432_19 = new hardalgo19();
  hashable6432 hardalgo6432_20 = new hardalgo20();
  hashable6432 hardalgo6432_21 = new hardalgo21();
  hashable6432 hardalgo6432_22 = new hardalgo22();
  hashable6432 hardalgo6432_23 = new hardalgo23();
  for (indx=0;indx<99999;++indx)
  {
    t ^= measure6432(hardalgo6432_19, 2);
    t ^= measure6432(hardalgo6432_20, 2);
    t ^= measure6432(hardalgo6432_21, 2);
    t ^= measure6432(hardalgo6432_22, 2);
    t ^= measure6432(hardalgo6432_23, 2);
  }
  System.out.println(t);
  System.out.println("measure for 64->32 algo19 shift 1");
  for (indx=0;indx<5;++indx)
    System.out.println(measure6432(hardalgo6432_19,99999000));
  System.out.println("measure for 64->32 algo20 shift 2");
  for (indx=0;indx<5;++indx)
    System.out.println(measure6432(hardalgo6432_20,99999000));
  System.out.println("measure for 64->32 algo21 shift 3");
  for (indx=0;indx<5;++indx)
    System.out.println(measure6432(hardalgo6432_21,99999000));
  System.out.println("measure for 64->32 mult shift 1");
  for (indx=0;indx<5;++indx)
    System.out.println(measure6432(hardalgo6432_22,99999000));
  System.out.println("measure for 64->32 mult 2");
  for (indx=0;indx<5;++indx)
    System.out.println(measure6432(hardalgo6432_23,99999000));

  emit6432(hardalgo6432_19, "algo19.txt");
  // emit6432(hardalgo6432_20, "algo20.txt");
  // emit6432(hardalgo6432_21, "algo21.txt");
  // emit6432(hardalgo6432_22, "algo22.txt");
  // emit6432(hardalgo6432_23, "algo23.txt");
*/
/*
  System.out.println("64 algorithm algo9a shift 0");
  driver64(new algo9a(), .11f, 33,33,11,16,11,33,33);
  System.out.println("64 algorithm algo9 shift 0");
  driver64(new algo9(), .11f, 33,33,11,16,11,33,33);
  System.out.println("64 algorithm shift 1");
  // driver64(new algo10(), .17f, 33,27,20,16,20,27,33);
  System.out.println("64 algorithm shift 2");
//  driver64(new algo11(), .20f, 33,33,19,15,19,33,33);
  System.out.println("64 algorithm mult 1");
//  driver64(new algo12(), .17f, 33,27,20,10,20,27,33);
  System.out.println("64 algorithm mult 2");
//  driver64(new algo13(), .17f, 33,27,20,10,20,27,33);
  hashable64 myhardalgo9a = new hardalgo9a();
  hashable64 myhardalgo10 = new hardalgo10();
  hashable64 myhardalgo11 = new hardalgo11();
  hashable64 myhardalgo12 = new hardalgo12();
  hashable64 myhardalgo13 = new hardalgo13();

  emit64(myhardalgo9a, "algo9a.txt");

  for (indx=0;indx<99999;++indx)
  {
    t ^= measure64(myhardalgo9a, 2);
    t ^= measure64(myhardalgo10, 2);
    t ^= measure64(myhardalgo11, 2);
    t ^= measure64(myhardalgo12, 2);
    t ^= measure64(myhardalgo13, 2);
  }
  System.out.println(t);
  System.out.println("measure for 64 shift 0");
  for (indx=0;indx<5;++indx)
    System.out.println(measure64(myhardalgo9a,99999000));
  System.out.println("measure for 64 shift 1");
  for (indx=0;indx<5;++indx)
    System.out.println(measure64(myhardalgo10,99999000));
  System.out.println("measure for 64 shift 2");
  for (indx=0;indx<5;++indx)
    System.out.println(measure64(myhardalgo11,99999000));
  System.out.println("measure for 64 mult 1");
  for (indx=0;indx<5;++indx)
    System.out.println(measure64(myhardalgo12,99999000));
  System.out.println("measure for 64 mult shift 2");
  for (indx=0;indx<5;++indx)
    System.out.println(measure64(myhardalgo13,99999000));
*/

  hashable32 myhardalgo00 = new hardalgo00();
  emit32(myhardalgo00, "algo00.txt");

  System.out.println("algorithm 0");
  driver32(new algo0(), .17f,  8, 17,16,13,13,16,17);
  System.out.println("algorithm 1");
  driver32(new algo1(), .26f,  1, 17,16,13,13,16,17);
  System.out.println("algorithm a");
  driver32(new algo_a(), .17f,  1, 17,16,4,13,16,17);
  System.out.println("algorithm b");
  driver32(new algo_b(), .17f,  1, 17,16,4,13,16,17);
  System.out.println("algorithm c");
  driver32(new algo_c(), .17f,  1, 17,16,4,13,16,17);
  System.out.println("algorithm d");
  driver32(new algo_d(), .17f,  1, 17,16,4,13,16,17);
  System.out.println("algorithm e");
  driver32(new algo_e(), .15f,  1, 17,16,13,13,16,17);
  System.out.println("algorithm f");
  driver32(new algo_f(), .15f,  1, 17,16,13,13,16,17);
  System.out.println("algorithm g");
  driver32(new algo_g(), .15f,  1, 17,16,13,13,16,17);
  System.out.println("algorithm h");
  driver32(new algo_h(), .15f,  1, 17,16,13,13,16,17);
  System.out.println("algorithm 4 : double not");
  driver32(new algo4(), .23f,  1, 1,16,13,13,16,17);
  System.out.println("algorithm 2 mult doubled");
  // driver32(new algo2(), .17f, 1,  17,3,4,4,3,17);
  System.out.println("algorithm 3 mult");
  hashable32 myalgo3 = new algo3();
  driver32(new algo3(), .17f, 64 ,17,3,4,12,3,17);
  System.out.println("algorithm 5 rot1");
//  driver32(new algo5(), .17f, 1, 20,20,4,20,4,17);
  System.out.println("algorithm 6 rot2");
//  driver32(new algo6(), .17f, 1, 20,20,4,20,4,17);
  System.out.println("algorithm 7: 7 rounds");
  driver32(new algo7(), .13f,  17,17,4,14,4,14,17);
  hashable32 myhardalgo0 = new hardalgo0();
  hashable32 myhardalgo1 = new hardalgo1();
  hashable32 myhardalgo_a = new hardalgo_a();
  hashable32 myhardalgo2 = new hardalgo2();
  hashable32 myhardalgo3 = new hardalgo3();
  hashable32 myhardalgo4 = new hardalgo4();
  hashable32 myhardalgo5 = new hardalgo5();
  hashable32 myhardalgo7 = new hardalgo7();

  emit32(myhardalgo0, "algo0.txt");
  emit32(myhardalgo_a, "algoa.txt");

  for (indx=0;indx<99999;++indx)
  {
    t ^= measure32(myhardalgo00, 2);
    t ^= measure32(myhardalgo0, 2);
    t ^= measure32(myhardalgo1, 2);
    t ^= measure32(myhardalgo_a, 2);
    t ^= measure32(myhardalgo2, 2);
    t ^= measure32(myhardalgo3, 2);
    t ^= measure32(myhardalgo4, 2);
    t ^= measure32(myhardalgo5, 2);
    t ^= measure32(myhardalgo7, 2);
  }
  System.out.println(t);
  System.out.println("measure for algo00");
  for (indx=0;indx<5;++indx)
  {
    System.out.println(measure32(myhardalgo00,99999000));
  }
  System.out.println("measure for algo0");
  for (indx=0;indx<5;++indx)
  {
    System.out.println(measure32(myhardalgo0,99999000));
  }
  System.out.println("measure for algo1");
  for (indx=0;indx<5;++indx)
  {
    System.out.println(measure32(myhardalgo1,99999000));
  }
  System.out.println("measure for algo a");
  for (indx=0;indx<5;++indx)
  {
    System.out.println(measure32(myhardalgo_a,99999000));
  }
  System.out.println("measure for algo3");
  for (indx=0;indx<5;++indx)
  {
    System.out.println(measure32(myhardalgo3,99999000));
  }
  System.out.println("measure for algo4");
  for (indx=0;indx<5;++indx)
  {
    System.out.println(measure32(myhardalgo4,99999000));
  }
  System.out.println("measure for algo5");
  for (indx=0;indx<5;++indx)
  {
    System.out.println(measure32(myhardalgo5,99999000));
  }
  System.out.println("measure for algo7");
  for (indx=0;indx<5;++indx)
  {
    System.out.println(measure32(myhardalgo7,99999000));
  }


 } /* end of main() */

}



