using System;
using System.Collections.Generic;
using System.Text;

namespace RaptorDB
{
    public class MurmurHash2Unsafe
    {
        public UInt32 Hash(Byte[] data)
        {
            return Hash(data, 0xc58f1a7b);
            //return KMurmurHash3(data, 0xde14f0ce);
            //return OneAtATime(data, 0xde14f0ce);
        }
        const UInt32 m = 0x5bd1e995;
        const Int32 r = 24;

        public static UInt32 Hash1(UInt32 a)
        {
            a = (a ^ 61) ^ (a >> 16);
            a = a + (a << 3);
            a = a ^ (a >> 4);
            a = a * 0x27d4eb2d;
            a = a ^ (a >> 15);
            return a;
            
        }

        public static UInt32 Hash0(UInt32 a)
        {
            a = (a + 0x7ed55d16) + (a << 12);
            a = (a ^ 0xc761c23c) ^ (a >> 19);
            a = (a + 0x165667b1) + (a << 5);
            a = (a + 0xd3a2646c) ^ (a << 9);
            a = (a + 0xfd7046c5) + (a << 3);
            a = (a ^ 0xb55a4f09) ^ (a >> 16);
            return a;
        }

        public unsafe UInt32 Hash(Byte[] data, UInt32 seed)
        {
            Int32 length = data.Length;
            if (length == 0)
                return 0;
            UInt32 h = seed ^ (UInt32)length;
            Int32 remainingBytes = length & 3; // mod 4
            Int32 numberOfLoops = length >> 2; // div 4
            fixed (byte* firstByte = &(data[0]))
            {
                UInt32* realData = (UInt32*)firstByte;
                while (numberOfLoops != 0)
                {
                    UInt32 k = *realData;
                    k *= m;
                    k ^= k >> r;
                    k *= m;

                    h *= m;
                    h ^= k;
                    numberOfLoops--;
                    realData++;
                }
                switch (remainingBytes)
                {
                    case 3:
                        h ^= (UInt16)(*realData);
                        h ^= ((UInt32)(*(((Byte*)(realData)) + 2))) << 16;
                        h *= m;
                        break;
                    case 2:
                        h ^= (UInt16)(*realData);
                        h *= m;
                        break;
                    case 1:
                        h ^= *((Byte*)realData);
                        h *= m;
                        break;
                    default:
                        break;
                }
            }

            // Do a few final mixes of the hash to ensure the last few
            // bytes are well-incorporated.
            h ^= h >> 13;
            h *= m;
            h ^= h >> 15;

            return h;
        }
        private static UInt32 rotl32(UInt32 x, byte r)
        {
            return (x << r) | (x >> (32 - r));
        }

        private static UInt32 fmix(UInt32 h)
        {
            h ^= h >> 16;
            h *= 0x85ebca6b;
            h ^= h >> 13;
            h *= 0xc2b2ae35;
            h ^= h >> 16;
            return h;
        }

        public static UInt32 MurmurHash3(Byte[] data, UInt32 seed)
        {
            const UInt32 c1 = 0xcc9e2d51;
            const UInt32 c2 = 0x1b873593;


            int curLength = data.Length;    /* Current position in byte array */
            int length = curLength;   /* the const length we need to fix tail */
            UInt32 h1 = seed;
            UInt32 k1 = 0;

            /* body, eat stream a 32-bit int at a time */
            Int32 currentIndex = 0;
            while (curLength >= 4)
            {
                /* Get four bytes from the input into an UInt32 */
                k1 = (UInt32)(data[currentIndex++]
                  | data[currentIndex++] << 8
                  | data[currentIndex++] << 16
                  | data[currentIndex++] << 24);

                /* bitmagic hash */
                k1 *= c1;
                k1 = rotl32(k1, 15);
                k1 *= c2;

                h1 ^= k1;
                h1 = rotl32(h1, 13);
                h1 = h1 * 5 + 0xe6546b64;
                curLength -= 4;
            }

            /* tail, the reminder bytes that did not make it to a full int */
            /* (this switch is slightly more ugly than the C++ implementation 
             * because we can't fall through) */
            switch (curLength)
            {
                case 3:
                    k1 = (UInt32)(data[currentIndex++]
                      | data[currentIndex++] << 8
                      | data[currentIndex++] << 16);
                    k1 *= c1;
                    k1 = rotl32(k1, 15);
                    k1 *= c2;
                    h1 ^= k1;
                    break;
                case 2:
                    k1 = (UInt32)(data[currentIndex++]
                      | data[currentIndex++] << 8);
                    k1 *= c1;
                    k1 = rotl32(k1, 15);
                    k1 *= c2;
                    h1 ^= k1;
                    break;
                case 1:
                    k1 = (UInt32)(data[currentIndex++]);
                    k1 *= c1;
                    k1 = rotl32(k1, 15);
                    k1 *= c2;
                    h1 ^= k1;
                    break;
            };

            // finalization, magic chants to wrap it all up
            h1 ^= (UInt32)length;
            h1 = fmix(h1);

            unchecked
            {
                return (UInt32)(Int32)h1;
            }
        }
        //http://www.team5150.com/~andrew/noncryptohashzoo/
        public unsafe UInt32 OneAtATime(Byte[] data, UInt32 seed)
        {
            int curLength = data.Length;
            if (curLength == 0)
                return 0;
            UInt32 hash = seed + (UInt32)curLength;
            fixed (byte* firstByte = &(data[0]))
            {

                byte* realData = (byte*)firstByte;
                while (curLength>0)
                {
                    hash += (*realData++);
                    hash += (hash << 10);
                    hash ^= (hash >> 6);
                    curLength--;
                }
                hash += (hash << 3);
                hash ^= (hash >> 11);
                hash += (hash << 15);
            }
            return hash;
        }

        public unsafe UInt32 KMurmurHash3(Byte[] data, UInt32 seed)
        {
            const UInt32 c1 = 0xcc9e2d51;
            const UInt32 c2 = 0x1b873593;


            int curLength = data.Length;    /* Current position in byte array */
            int length = curLength;   /* the const length we need to fix tail */
            UInt32 h1 = seed;
            UInt32 k1 = 0;
            if (curLength == 0)
                return 0;
            /* body, eat stream a 32-bit int at a time */
            Int32 remainingBytes = length & 3; // mod 4
            Int32 numberOfLoops = length >> 2; // div 4
            fixed (byte* firstByte = &(data[0]))
            {

                UInt32* realData = (UInt32*)firstByte;
                while (numberOfLoops != 0)
                {
                    k1 = *realData;
                    /* bitmagic hash */
                    k1 *= c1;
                    k1 = (k1 << 15) | (k1 >> 17);
                    k1 *= c2;

                    h1 ^= k1;
                    h1 = (h1 << 13) | (h1 >> 19);
                    h1 = h1 * 5 + 0xe6546b64;
                    numberOfLoops--;
                    realData++;
                }
            


                /* tail, the reminder bytes that did not make it to a full int */
                /* (this switch is slightly more ugly than the C++ implementation 
                 * because we can't fall through) */
                switch (remainingBytes)
                {
                    case 3:
                        k1 = (UInt16)(*realData)
                          | ((UInt32)(*(((Byte*)(realData)) + 2))) << 16;
                        k1 *= c1;
                        k1 =(k1 << 15) | (k1 >> 17);
                        k1 *= c2;
                        h1 ^= k1;
                        break;
                    case 2:
                        k1 = (UInt16)(*realData);
                        k1 *= c1;
                        k1 =(k1 << 15) | (k1 >> 17);
                        k1 *= c2;
                        h1 ^= k1;
                        break;
                    case 1:
                        k1 = *((Byte*)realData);
                        k1 *= c1;
                        k1 =(k1 << 15) | (k1 >> 17);
                        k1 *= c2;
                        h1 ^= k1;
                        break;
                }
            }
            // finalization, magic chants to wrap it all up
            h1 ^= (UInt32)length;
            h1 ^= h1 >> 16;
            h1 *= 0x85ebca6b;
            h1 ^= h1 >> 13;
            h1 *= 0xc2b2ae35;
            h1 ^= h1 >> 16;

            return (UInt32)(Int32)h1;

        }
    }
}
