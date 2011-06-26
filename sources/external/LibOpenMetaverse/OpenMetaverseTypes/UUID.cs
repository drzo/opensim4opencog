#define INTERNABLE_UUIDS
#define USE_REAL_FACTORY

/*
 * Copyright (c) 2008, openmetaverse.org
 * All rights reserved.
 *
 * - Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer.
 * - Neither the name of the openmetaverse.org nor the names
 *   of its contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

using System;
#if !(USE_UUID_INTERFACE)
using System.Collections.Generic;
using System.Reflection;
using System.Runtime.Serialization;
using System.Text;
using UUIDCantBeNull = OpenMetaverse.UUID;
#endif

namespace OpenMetaverse
{
    public class WasAStruct
    {
        public override string ToString()
        {
            return StructToString(this);
        }

        public static string StructToString(object t)
        {
            StringBuilder result = new StringBuilder();
            Type structType = t.GetType();
            FieldInfo[] fields = structType.GetFields();

            foreach (FieldInfo field in fields)
            {
                result.Append(field.Name + ": " + field.GetValue(t) + " ");
            }
            result.AppendLine();
            return result.ToString().TrimEnd();
        }
    }
#if (INTERNABLE_UUIDS)

#if USE_UUID_INTERFACE
    public interface UUID
    {
        Guid GetGuid();
        byte[] GetBytes();
    }
#endif

        // The following extension methods can be accessed by instances of any 
        // class that implements IMyInterface.
    public static class LL_UUID
    {

        public static void MethodA(this UUIDCantBeNull myInterface, int i)
        {
            Console.WriteLine
                ("Extension.MethodA(this OkAsNullUUID myInterface, int i)");
        }

        public static void MethodA(this UUIDCantBeNull myInterface, string s)
        {
            Console.WriteLine
                ("Extension.MethodA(this IMyInterface myInterface, string s)");
        }

        // This method is never called in ExtensionMethodsDemo1, because each 
        // of the three classes A, B, and C implements a method named MethodB
        // that has a matching signature.
        public static void MethodB(this UUIDCantBeNull myInterface)
        {
            Console.WriteLine
                ("Extension.MethodB(this IMyInterface myInterface)");
        }
    }
#endif

    [Serializable]
    public class UUIDFactory
    {
        public static UUID GetUUID(byte[] data, int pos)
        {
#if INTERNABLE_UUIDS
            return GetUUID(UUID.GuidFromBytes(data, pos));
#else
            return new UUIDCantBeNull(data, pos);
#endif
        }
        public static UUIDCantBeNull GetUUID(string s)
        {
            return GetUUID(UUID.GuidFromString(s));
        }
#if !(USE_REAL_FACTORY)
        public static UUID GetUUID(UUID s)
        {
            return new UUIDCantBeNull(s, true);
        }
#else
        static readonly Dictionary<Guid,UUID> GuidToUUID = new Dictionary<Guid, UUID>();
#endif

        public static UUID GetUUID(Guid s)
        {
            lock (GuidToUUID)
            {
                UUID u;
                if (!GuidToUUID.TryGetValue(s, out u))
                {
                    u = GuidToUUID[s] = new UUID(s);
                }
                return u;
            }
            //return new UUIDCantBeNull(s ,true);
        }
        public static UUID GetUUID(ulong s)
        {
            return GetUUID(UUID.GuidFromLong(s));
        }

        /// <summary>A cache of UUID.Zero as a string to optimize a common path</summary>
        private static readonly string ZeroString = Guid.Empty.ToString();


        /// <summary>An UUID with a value of all zeroes</summary>
        public static readonly UUIDCantBeNull Zero = UUIDFactory.GetUUID(ZeroString);

    }
    /// <summary>
    /// A 128-bit Universally Unique Identifier, used throughout the Second
    /// Life networking protocol
    /// </summary>
    [Serializable]
#if INTERNABLE_UUIDS
#if USE_UUID_INTERFACE
    public class UUIDCantBeNull : IComparable<UUID>, IEquatable<UUID>, UUID
#else
    public class UUID : UUIDFactory, IComparable<UUID>, IEquatable<UUID>
#endif
#else
    public struct UUID : IComparable<UUID>, IEquatable<UUID>
#endif
    {
        /// <summary>The System.Guid object this struct wraps around</summary>
#if INTERNABLE_UUIDS
        readonly internal Guid _Guid;
#else
        public Guid _Guid;
#endif
        public Guid GetGuid()
        {
            return _Guid;
        }

        public static Guid GuidFromLong(ulong val)
        {
            byte[] end = BitConverter.GetBytes(val);
            if (!BitConverter.IsLittleEndian)
                Array.Reverse(end);

            return new Guid(0, 0, 0, end);
        }

        public static Guid GuidFromString(string val)
        {
            return new Guid(val);
        }

        public static UUID UUIDFromString(string val)
        {
            return GetUUID(val);
        }

        #region Constructors

        /// <summary>
        /// Constructor that takes a string UUID representation
        /// </summary>
        /// <param name="val">A string representation of a UUID, case 
        /// insensitive and can either be hyphenated or non-hyphenated</param>
        /// <example>UUID("11f8aa9c-b071-4242-836b-13b7abe0d489")</example>
#if USE_UUID_INTERFACE
        public UUIDCantBeNull(string val)
#else
        public UUID(string val)
#endif
        {
            if (String.IsNullOrEmpty(val))
            {
                toStringCache = ZeroString;
                _Guid = new Guid();
            }
            else
            {
                _Guid = new Guid(val);
                toStringCache = val;
            }
        }

        /// <summary>
        /// Constructor that takes a System.Guid object
        /// </summary>
        /// <param name="val">A Guid object that contains the unique identifier
        /// to be represented by this UUID</param>
#if USE_UUID_INTERFACE
        public UUIDCantBeNull(UUID val)
        {
            _Guid = val.GetGuid();
        }
#else
        public UUID(Guid val)
        {
            _Guid = val;
            toStringCache = null;
        }
#endif

//#if !(USE_REAL_FACTORY)
        /// <summary>
        /// Constructor that takes a byte array containing a UUID
        /// </summary>
        /// <param name="source">Byte array containing a 16 byte UUID</param>
        /// <param name="pos">Beginning offset in the array</param>
#if USE_UUID_INTERFACE
        public UUIDCantBeNull(byte[] source, int pos)
#else
        public  UUID(byte[] source, int pos)
#endif
        {
            //_Guid = Zero.GetGuid();
            toStringCache = null;
            _Guid = GuidFromBytes(source, pos);
        }

        /// <summary>
        /// Constructor that takes an unsigned 64-bit unsigned integer to 
        /// convert to a UUID
        /// </summary>
        /// <param name="val">64-bit unsigned integer to convert to a UUID</param>
#if USE_UUID_INTERFACE
        public UUIDCantBeNull(ulong val)
#else
        public UUID(ulong val)
#endif
        {
            byte[] end = BitConverter.GetBytes(val);
            if (!BitConverter.IsLittleEndian)
                Array.Reverse(end);

            toStringCache = null;
            _Guid = new Guid(0, 0, 0, end);
        }

#if USE_UUID_INTERFACE
        public UUIDCantBeNull(Guid source)
        {
            _Guid = source;
        }
#endif

        /// <summary>
        /// Copy constructor
        /// </summary>
        /// <param name="val">UUID to copy</param>
#if USE_UUID_INTERFACE
        public UUIDCantBeNull(UUIDCantBeNull val)
#else
        internal UUID(UUID val)
#endif
        {
            _Guid = val._Guid;
            toStringCache = null;
        }
//#endif //USE_REAL_FACTORY
        #endregion Constructors

        #region Public Methods

        /// <summary>
        /// IComparable.CompareTo implementation
        /// </summary>
        public int CompareTo(UUID id)
        {
            return _Guid.CompareTo(id.GetGuid());
        }

        /// <summary>
        /// Assigns this UUID from 16 bytes out of a byte array
        /// </summary>
        /// <param name="source">Byte array containing the UUID to assign this UUID to</param>
        /// <param name="pos">Starting position of the UUID in the byte array</param>
        ///         public void FromUUIDBytes(byte[] source, int pos)
        //{
       //     _Guid = GuidFromBytes(source, pos);
      //  }

        /// 
        static public Guid GuidFromBytes(byte[] source, int pos)
        {
            int a = (source[pos + 0] << 24) | (source[pos + 1] << 16) | (source[pos + 2] << 8) | source[pos + 3];
            short b = (short)((source[pos + 4] << 8) | source[pos + 5]);
            short c = (short)((source[pos + 6] << 8) | source[pos + 7]);

            var _Guid = new Guid(a, b, c, source[pos + 8], source[pos + 9], source[pos + 10], source[pos + 11],
                source[pos + 12], source[pos + 13], source[pos + 14], source[pos + 15]);
            return _Guid;
        }
        /// <summary>
        /// Returns a copy of the raw bytes for this UUID
        /// </summary>
        /// <returns>A 16 byte array containing this UUID</returns>
        public byte[] GetBytes()
        {
            byte[] output = new byte[16];
            ToBytes(output, 0);
            return output;
        }

        /// <summary>
        /// Writes the raw bytes for this UUID to a byte array
        /// </summary>
        /// <param name="dest">Destination byte array</param>
        /// <param name="pos">Position in the destination array to start
        /// writing. Must be at least 16 bytes before the end of the array</param>
        public void ToBytes(byte[] dest, int pos)
        {
            byte[] bytes = _Guid.ToByteArray();
            dest[pos + 0] = bytes[3];
            dest[pos + 1] = bytes[2];
            dest[pos + 2] = bytes[1];
            dest[pos + 3] = bytes[0];
            dest[pos + 4] = bytes[5];
            dest[pos + 5] = bytes[4];
            dest[pos + 6] = bytes[7];
            dest[pos + 7] = bytes[6];
            Buffer.BlockCopy(bytes, 8, dest, pos + 8, 8);
        }

        /// <summary>
        /// Calculate an LLCRC (cyclic redundancy check) for this UUID
        /// </summary>
        /// <returns>The CRC checksum for this UUID</returns>
        public uint CRC()
        {
            uint retval = 0;
            byte[] bytes = GetBytes();

            retval += (uint)((bytes[3] << 24) + (bytes[2] << 16) + (bytes[1] << 8) + bytes[0]);
            retval += (uint)((bytes[7] << 24) + (bytes[6] << 16) + (bytes[5] << 8) + bytes[4]);
            retval += (uint)((bytes[11] << 24) + (bytes[10] << 16) + (bytes[9] << 8) + bytes[8]);
            retval += (uint)((bytes[15] << 24) + (bytes[14] << 16) + (bytes[13] << 8) + bytes[12]);

            return retval;
        }

        /// <summary>
        /// Create a 64-bit integer representation from the second half of this UUID
        /// </summary>
        /// <returns>An integer created from the last eight bytes of this UUID</returns>
        public ulong GetULong()
        {
            byte[] bytes = _Guid.ToByteArray();

            return (ulong)
                ((ulong)bytes[8] +
                ((ulong)bytes[9] << 8) +
                ((ulong)bytes[10] << 16) +
                ((ulong)bytes[12] << 24) +
                ((ulong)bytes[13] << 32) +
                ((ulong)bytes[13] << 40) +
                ((ulong)bytes[14] << 48) +
                ((ulong)bytes[15] << 56));
        }

        #endregion Public Methods

        #region Static Methods

        /// <summary>
        /// Generate a UUID from a string
        /// </summary>
        /// <param name="val">A string representation of a UUID, case 
        /// insensitive and can either be hyphenated or non-hyphenated</param>
        /// <example>UUID.Parse("11f8aa9c-b071-4242-836b-13b7abe0d489")</example>
        public static UUID Parse(string val)
        {
            return UUIDFactory.GetUUID(val);
        }

        /// <summary>
        /// Generate a UUID from a string
        /// </summary>
        /// <param name="val">A string representation of a UUID, case 
        /// insensitive and can either be hyphenated or non-hyphenated</param>
        /// <param name="result">Will contain the parsed UUID if successful,
        /// otherwise null</param>
        /// <returns>True if the string was successfully parse, otherwise false</returns>
        /// <example>UUID.TryParse("11f8aa9c-b071-4242-836b-13b7abe0d489", result)</example>
        public static bool TryParse(string val, out UUID result)
        {
            if (!CanBeUUID(val))
            {
                result = Zero;
                return false;
            }

            try
            {
                result = Parse(val);
                return true;
            }
            catch (Exception)
            {
                result = Zero;
                return false;
            }
        }

        private static bool CanBeUUID(string val)
        {

            if (String.IsNullOrEmpty(val))
            {
                return false;
            }
            int valLength = val.Length;
            if ((val[0] == '{' && valLength != 38) ||
                (valLength != 36 && valLength != 32))
            {
                return false;
            }
            int fd = val.IndexOfAny(" ._@".ToCharArray());
            if (fd != -1) return false;

            fd = val.IndexOf('-');
            if (valLength == 32 && fd == -1)
            {
                return false;
            }
            if (valLength == 38 || fd != 8)
            {
                return false;
            }
            return true;
        }

        /// <summary>
        /// Combine two UUIDs together by taking the MD5 hash of a byte array
        /// containing both UUIDs
        /// </summary>
        /// <param name="first">First UUID to combine</param>
        /// <param name="second">Second UUID to combine</param>
        /// <returns>The UUID product of the combination</returns>
        public static UUID Combine(UUID first, UUID second)
        {
            // Construct the buffer that MD5ed
            byte[] input = new byte[32];
            Buffer.BlockCopy(first.GetBytes(), 0, input, 0, 16);
            Buffer.BlockCopy(second.GetBytes(), 0, input, 16, 16);

            return UUIDFactory.GetUUID(Utils.MD5(input), 0);
        }

        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        public static UUID Random()
        {
            return UUIDFactory.GetUUID(Guid.NewGuid());
        }

        #endregion Static Methods

        #region Overrides

        /// <summary>
        /// Return a hash code for this UUID, used by .NET for hash tables
        /// </summary>
        /// <returns>An integer composed of all the UUID bytes XORed together</returns>
        public override int GetHashCode()
        {
            return _Guid.GetHashCode();
        }

        /// <summary>
        /// Comparison function
        /// </summary>
        /// <param name="o">An object to compare to this UUID</param>
        /// <returns>True if the object is a UUID and both UUIDs are equal</returns>
        public override bool Equals(object o)
        {

#if INTERNABLE_UUIDS
            if (o == null) o = Zero;
            else
#endif
            if (!(o is UUID)) return false;

            UUID uuid = (UUID)o;
            if (ReferenceEquals(o, this)) return true;
            if (_Guid != uuid.GetGuid()) return false;
            return true;
        }

        /// <summary>
        /// Comparison function
        /// </summary>
        /// <param name="uuid">UUID to compare to</param>
        /// <returns>True if the UUIDs are equal, otherwise false</returns>
        public bool Equals(UUID uuid)
        {
#if INTERNABLE_UUIDS
            uuid = uuid ?? Zero;
#endif
            return _Guid == uuid.GetGuid();
        }

        private string toStringCache;

        /// <summary>
        /// Get a hyphenated string representation of this UUID
        /// </summary>
        /// <returns>A string representation of this UUID, lowercase and 
        /// with hyphens</returns>
        /// <example>11f8aa9c-b071-4242-836b-13b7abe0d489</example>
        public override string ToString()
        {
            if (_Guid == Guid.Empty)
                return ZeroString;
            else
                if (toStringCache != null) return toStringCache;
            toStringCache = string.Intern(_Guid.ToString());
            return toStringCache;
        }

        #endregion Overrides

        #region Operators

        /// <summary>
        /// Equals operator
        /// </summary>
        /// <param name="lhs">First UUID for comparison</param>
        /// <param name="rhs">Second UUID for comparison</param>
        /// <returns>True if the UUIDs are byte for byte equal, otherwise false</returns>
        public static bool operator ==(UUIDCantBeNull lhs, UUIDCantBeNull rhs)
        {
#if INTERNABLE_UUIDS
            lhs = ZeroCheck(lhs);
            rhs = ZeroCheck(rhs);
#endif
            return lhs.GetGuid() == rhs.GetGuid();
        }

        private static UUID ZeroCheck(UUID lhs)
        {
            if (!ReferenceEquals(lhs, null)) return lhs;
            return Zero;
        }

        /// <summary>
        /// Not equals operator
        /// </summary>
        /// <param name="lhs">First UUID for comparison</param>
        /// <param name="rhs">Second UUID for comparison</param>
        /// <returns>True if the UUIDs are not equal, otherwise true</returns>
        public static bool operator !=(UUIDCantBeNull lhs, UUIDCantBeNull rhs)
        {
            return !(lhs == rhs);
        }

        /// <summary>
        /// XOR operator
        /// </summary>
        /// <param name="lhs">First UUID</param>
        /// <param name="rhs">Second UUID</param>
        /// <returns>A UUID that is a XOR combination of the two input UUIDs</returns>
        public static UUID operator ^(UUIDCantBeNull lhs, UUIDCantBeNull rhs)
        {
#if INTERNABLE_UUIDS
            lhs = ZeroCheck(lhs);
            rhs = ZeroCheck(rhs);
#endif
            byte[] lhsbytes = lhs.GetBytes();
            byte[] rhsbytes = rhs.GetBytes();
            byte[] output = new byte[16];

            for (int i = 0; i < 16; i++)
            {
                output[i] = (byte)(lhsbytes[i] ^ rhsbytes[i]);
            }

            return UUIDFactory.GetUUID(output, 0);
        }

        /// <summary>
        /// String typecasting operator
        /// </summary>
        /// <param name="val">A UUID in string form. Case insensitive, 
        /// hyphenated or non-hyphenated</param>
        /// <returns>A UUID built from the string representation</returns>
        public static explicit operator UUIDCantBeNull(string val)
        {
            return UUIDFactory.GetUUID(val);
        }

#if PERFECT_WORLD
        public static implicit operator UUID(object val)
        {
            if (val == null) return UUID.Zero;
            return (UUID) val;
        }
#warning OMG it is a perfect world!
#endif
        #endregion Operator

        /// <summary>A cache of UUID.Zero as a string to optimize a common path</summary>
        private static readonly string ZeroString = Guid.Empty.ToString();


        /// <summary>An UUID with a value of all zeroes</summary>
        public static readonly UUIDCantBeNull Zero = UUIDFactory. GetUUID(ZeroString);

        [NonSerialized]
        public object ExternalData;
    }
}
