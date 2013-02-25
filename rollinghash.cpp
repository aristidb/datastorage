#include <stdint.h>
#include <iostream>
#include <stdexcept>
#include <typeinfo>
#include <random>

// gcc actually recognizes this as a rotate-left
#define ROT64(x, i) ((uint64_t((x)) << (i)) | (uint64_t((x)) >> (64 - (i))))

template<typename T>
struct ihash {};

template<>
struct ihash<uint8_t> {
  static uint64_t tab[256];

  static uint64_t hash(uint8_t byte) {
    return tab[byte];
  };

  static uint8_t reverse(uint64_t h) {
    for (int i = 0; i < 256; ++i)
      if (tab[i] == h)
        return uint8_t(i);
    throw std::runtime_error("Invalid hash");
  };
};

// putStrLn =<< (intercalate ",\n" . map (\[a,b,c,d] -> printf "  0x%0.16x, 0x%0.16x, 0x%0.16x, 0x%0.16x" a b c d :: String) <$> replicateM 64 (replicateM 4 (randomIO :: IO Word64)))
uint64_t ihash<uint8_t>::tab[256] = {
  0xfead5b707dc7705c, 0x377c1e06dc1e45cf, 0x0184179586d5ae76, 0xd23aa044f8193aa6,
  0xbd8ef5fcde7bd95e, 0x29a822b00a75ea90, 0x5ba03c1b2fdc2f86, 0x4f67d80bad410270,
  0xfcc4b6b0cb67bb75, 0x8f4359ea8777f5d0, 0x5a110ec6371430f5, 0xe15dae4e9709aa66,
  0xf8008efefbc29115, 0xd667f1ec0a5bcccf, 0xf56a2815cb29dbf8, 0xfc684381b1846cad,
  0x15be2b7ed5231683, 0x0c226b1f911732c5, 0xe449b5885fd1d930, 0xf6d86c27a242cf1d,
  0xc45cd857fdd32ca4, 0x36de9a99be8c3419, 0x8c34f2ee7050433f, 0x9fca9c3c61c05ce8,
  0xf3727fe070e026f1, 0x019172d2948847be, 0x547bcbfaaef62543, 0x97df2b9f6c70001b,
  0x2833bcc8513a3ae8, 0x63fb313206823b31, 0xe97ba036be16b286, 0xdde1143e9e8f7684,
  0xc4e0d8e6e491054c, 0x7c488b71d4cab5fd, 0x2e0986d1f8e2d362, 0x97978ef2435b5cb1,
  0x5506c42930dfc2a4, 0x3205d4c5886673cf, 0x22d5bfe8ba771cc5, 0x39503d4cb89afd6b,
  0x9fc1cad11e5b5755, 0xce967aae1fccb3c3, 0x0f164d18916d4043, 0x113893188dd7e602,
  0xedb9343bcf5f16ef, 0x0ce3b155cfc00cbf, 0x144c6c104492dff7, 0xe1ab67476c3c787f,
  0x06357095472efed7, 0x29573d1071bb1237, 0xe27e7ce26dcd54c6, 0x03a8623e84a0e89c,
  0x381194ea8cb4f8fe, 0x53cbe859f67ae71c, 0x85e08cd4432b31f5, 0x21f9758ed837239d,
  0x835aad64a22264b4, 0xf83ec94f1a0159f3, 0x016a63af376515a4, 0x8af0e82577e30b6e,
  0x4e3717bd197f8c8d, 0x20193e9699604cea, 0xfa8eb53310f65183, 0xb4980361f9a7e3b3,
  0xccb9771f04663244, 0xe975bed27cfd7a05, 0x46e9eb7dd3ffd802, 0xbe4b5af269bf4b64,
  0xf3ff155176f34e7e, 0xfc8e0e5c20814c3f, 0x3910b75cbe6677f5, 0x86fb5f8ef54ad45a,
  0xad430e08aa6aea72, 0x6a82d96b64a7f48c, 0x8aa8af38d787327e, 0x3a10990b93334a3d,
  0x010c38275c1fc3ac, 0x03eac81f78d29425, 0x356d5c2052ac066f, 0x9a14a98e51d4eafc,
  0x9c07059dac0831b5, 0x51c3aa247abb4a6c, 0x7f9d96a58c371a56, 0xe098fe430cedd615,
  0x960e3a1dbe524565, 0xf0401c427323f9e8, 0x2c22d55a6135331c, 0x8684f0837cb96bc9,
  0xb7ac495220cc7e9b, 0x4083bd84b86ad15e, 0xd7434c89c5464bfd, 0x6af7d49f8b658a9d,
  0x1b9878f861dd7d00, 0x6f5b7c3c3baaca36, 0xab6e6179f8b69d37, 0xda4c68dc98bcecd5,
  0x42cb0a636ba3fa38, 0x04f2814021d2f99a, 0x53a069c086a24b1d, 0xce39ccfebf0fd3cf,
  0x6da4576c5b938392, 0x73961a564d3684af, 0xe62079e996d934ab, 0x0fe43a5aded65708,
  0x8ea00b7c1ba4a6b4, 0xb3f325b0d1acc61a, 0xb7d68c724ee32277, 0x8838a5a56df55c2e,
  0x639e1a16eebc31dc, 0xc824476623ee8713, 0x6cfd35dd41d4a35f, 0xc0cf8dfb2eb67ca4,
  0x9b2db80c20bf8281, 0x8f43090206cbc3a5, 0xf018850ed3cce401, 0xdd9c873dbc24dacf,
  0x8c310f7792646b4a, 0xd64514da16af625c, 0x7de1ce3ec600b27d, 0x97742bfa39500785,
  0x37e57a0934a9e321, 0x0872c00e96058f5f, 0x9a9291f9452ff389, 0x1240421b117dc351,
  0x179b86fb16a829ed, 0xf773ae083f3cd908, 0xc32304664f8ce10f, 0x3bf48227de1ad114,
  0x1e051eb805a697ad, 0x70ef7848e5ff8346, 0xe7dc2e245cb0d7f3, 0xf9ccdaee9f48ca62,
  0x2ef082a2b4419ad3, 0x8a9287ed4ecd5750, 0x9f7b959d7c2ff1ba, 0x1b86fb1047eee0a9,
  0x342ac90ae166bfb4, 0x9c28c8b1a92ab370, 0x7248bbdc9faaf6c1, 0x1a3b7cdd85269cfa,
  0xcf3af94626a8c04f, 0x7a4ba5bef4f2856c, 0xa80dd551adc99bb8, 0xd7cef340d2e894ba,
  0x134c05ffd4928eba, 0x78b5ec19a589ff89, 0xd8d23f88cbd19231, 0xa95a65c91e98558c,
  0x0a996675f053aa80, 0xf35d31427e2dce98, 0x9985000bf5aefe7b, 0x73ac3f7cc6a7d6e0,
  0xc48e7406cc446de3, 0xf919aa74e167dbbe, 0x8b09e12fc48dbbcd, 0x33b5f1fbafc21ead,
  0xfbbf051f129c275d, 0x3239b2a8ee124a62, 0x8984d8d6b4a2ad6a, 0xe2293308df3acd2b,
  0x2529297459fa7fac, 0x36c72d90491e670f, 0xe7ca9d442ddae3b3, 0x8fa38fa2a012ce0d,
  0x5caac60bfad2c355, 0xa92158609a2e1285, 0x8a097df8fdb4c6cf, 0x1f1b6f8230e90ff0,
  0x8369b558bec7e569, 0x2e9d4f7aabcb210d, 0xafb7d767ac296c18, 0x2f5cd5c2515d81c1,
  0x45934465add6c676, 0xa5a45e774fd55fc3, 0xb4cb7e6e96d2a59d, 0xfdc6635b7ec562b0,
  0x1bcedbd2ea8b4bf9, 0x0ab5b24bb7ccf99d, 0x31aa59853645b444, 0x9149c77d16594ff5,
  0x77d9fee30ed21a67, 0x12982d192e88015b, 0xd45d1f788a6a1ef3, 0x332d24e0edf8ca0a,
  0x88c7695975b9d19b, 0x065f29d44b168060, 0xd2a2a94e28c99de3, 0x8235595da56d3cd7,
  0x4d58d451daa20b65, 0x36a1edff55e32372, 0xdf5c5c1e6b75c40b, 0x85c8a6bf77ecf2d3,
  0x7d72ca70ec2882f2, 0xfe286fdc7db952ca, 0x13e308d730603af4, 0x7803625ac832d270,
  0x920af03f31044bc6, 0x1fe4f2d207fecc03, 0x38537ef4067d59e9, 0x0eca7ef2100d20fc,
  0x0a6fe0e637366afc, 0x9d52574f1a85c9e5, 0xd0a0fdbd96ed75d2, 0xb6785ac2254c0d92,
  0x708122c30c97ff1f, 0x3e265597b73bc11e, 0xb3a870c781058225, 0xd93d905b304c4ec6,
  0x52ff676e213f6ccb, 0xde7503df2ee1aed6, 0xcc95a88b5c5fc05a, 0x351d157d93a36b21,
  0xf33702abc575fb08, 0xedcec8d4e602f76d, 0x210d869a4002a35a, 0xed6debb5a777ff11,
  0x4aa4ef159b20a5da, 0x1248e5993f479e53, 0x6afdd7979ed93e76, 0x0048f115bbf8fcc9,
  0x3c43dcc810f39b4c, 0xd596e217217ba38d, 0xf026411acc2ffc3f, 0xa9bfe1a48496f61c,
  0x3c5d0feeb403ffe6, 0xf3cc4252ff0a0e4b, 0x9287b23486699a23, 0xfcb9175b7c6ad7f1,
  0xd28a3ca6572757d0, 0x9679f32c0d2f01f8, 0x9aec761f1611e561, 0x00d429298b4544fc,
  0x08aa8292dcaff98f, 0x36c88c6cf0188da2, 0x674a34c8653e66b8, 0x0e6316f1731e02b8,
  0xd049743dfb30601e, 0xe99b74d4efa92d88, 0xa41f9c33729e01ad, 0xe0a2c2031c01b13d,
  0x04a396e2dfc4f61f, 0xdb2556b846e68964, 0xa5de683723f23704, 0x456f750ec820ea4f,
  0x47b81bb8dda336aa, 0x7c0b4cab5dffb32e, 0x020a4567a37ada75, 0x66325b8ce0bc3890,
  0x566e9b6d6a194d5e, 0x53efbb71787c4049, 0xee775a2e794219de, 0xa65c279ca51a6a46,
  0xaac56091fcec9c38, 0x0b34953b386ed0c4, 0xde46839785d1b945, 0x623a65d725974df2
};

template<>
struct ihash<uint64_t> {
  // derived from http://web.archive.org/web/20121102023700/http://www.concentric.net/~Ttwang/tech/inthash.htm
  static uint64_t hash(uint64_t key) {
    key = (~key) + (key << 21); // key = (key << 21) - key - 1;
    key = key ^ (key >> 24);
    key = (key + (key << 3)) + (key << 8); // key * 265
    key = key ^ (key >> 14);
    key = (key + (key << 2)) + (key << 4); // key * 21
    key = key ^ (key >> 28);
    key = key + (key << 31);
    return key;
  }
};

template<size_t n, bool divBy8 = (n % 8) == 0, bool divBy4 = (n % 4) == 0>
struct ghash;

template<>
struct ghash<0, true, true> {
  static uint64_t hash(uint8_t const *) {
    return 0;
  }
};

template<size_t n>
struct ghash<n, false, false> {
  static uint64_t hash(uint8_t const *buf) {
    return ROT64(ghash<n-1>::hash(buf), 1) ^ ihash<uint8_t>::hash(buf[n-1]);
  }
};

template<size_t n>
struct ghash<n, true, true> {
  static uint64_t hash(uint8_t const *buf) {
    return ROT64(ghash<n-8>::hash(buf), 1)
      ^ ihash<uint64_t>::hash(*reinterpret_cast<uint64_t const *>(buf+n-8));
  }
};

template<size_t n>
struct ghash<n, false, true> {
  static uint64_t hash(uint8_t const *buf) {
    return ROT64(ghash<n-4>::hash(buf), 1)
      ^ ihash<uint64_t>::hash(*reinterpret_cast<uint32_t const *>(buf+n-4));
  }
};

template<typename T, unsigned window>
class rhash {
private:
  uint64_t h;

public:
  explicit rhash(uint64_t seed) : h(seed) {}

  uint64_t add(T const &n) {
    h = ROT64(h, 1) ^ ihash<T>::hash(n);
    return h;
  }

  uint64_t add(T const *ns, uint64_t len) {
    while (len--) add(*++ns);
    return h;
  }

  uint64_t addRemove(T const &n, T const &o) {
    h = ROT64(h, 1) ^ ROT64(ihash<T>::hash(o), window % 64) ^ ihash<T>::hash(n);
    return h;
  }

  template<typename It>
  uint64_t hashAll(It start, It end) {
    It orig = start;
    for (unsigned n = 0; n < window; ++n) {
      if (start == end)
        break;
      add(*start++);
    }

    for (; start != end; ++start, ++orig)
      addRemove(*start, *orig);

    return h;
  }

  static void test() {
    std::cout << "testing rhash for " << typeid(T).name() << '/' << window << std::endl;

    std::default_random_engine gen;

    for (int i = 0; i < 20; ++i) {
      size_t n = std::uniform_int_distribution<size_t>(0, 100)(gen);
      size_t m = std::uniform_int_distribution<size_t>(window, 1000)(gen);

      std::uniform_int_distribution<T> dist;

      std::vector<T> a, b;
      while (n--)
        a.push_back(dist(gen));

      while (m--) {
        T const &v = dist(gen);
        a.push_back(v);
        b.push_back(v);
      }

      uint64_t h = rhash<T, window>(0).hashAll(a.begin(), a.end());
      uint64_t g = rhash<T, window>(0).hashAll(b.begin(), b.end());

      std::cout << (h==g) << " : " << h << '=' << g << std::endl;
    }
  }
};

template class ghash<13>;

//template uint64_t ihash<uint64_t>::hash(uint64_t);
//template class rhash<uint8_t, 128>;
//template class rhash<uint64_t, 16>;

int main() {
  //for (int i = 0; i < 256; ++i)
  //  if (ihash<uint8_t>::reverse(ihash<uint8_t>::hash(uint8_t(i))) != i)
  //    std::cerr << "uint8_t hash not reversible for: " << i << std::endl;
  //std::cout << "done.\n";

  //std::cout << "hash 4: " << ghash<4>::hash(reinterpret_cast<uint8_t const*>("abcd")) << std::endl;

/*
  rhash<uint8_t, 3>::test();
  rhash<uint8_t, 128>::test();
  rhash<uint64_t, 16>::test();
*/
}
