using System;
using System.IO;
using MushDLR223.Utilities;

namespace MushDLR223.Virtualization
{
    public class AutoClosingStream : Stream
    {
        public readonly Stream UnderlyingStream;
        public bool AutoClose = true;
        private string origname = "";

        private bool _isClosed;
        public bool IsClosed
        {
            get
            {
                if (_isClosed || UnderlyingStreamSetNull || (!CanRead && _isCloseable))
                {
                    return true;
                }
                return CanWrite;
            } 
            set
            {
                _isClosed = value;
            }
        }

        private bool _isCloseable;
        public bool UnderlyingStreamSetNull = false;

        public bool IsCloseable
        {
            get
            {
                return _isCloseable;
            }
            set
            {
                _isCloseable = value;
                bool cr = CanRead;
                bool we = CanSeek;
                // if (value && AutoClose)
                //Close();
            }
        }

        public AutoClosingStream(Stream stream)
        {
            UnderlyingStream = stream;
            var ss = stream.ToString();
            if (UnderlyingStream is FileStream)
            {
                var io = (System.IO.FileStream)stream;
                string s = io.Name;
                if (s == null || s.Trim().Length == 0)
                {
                    s = "" + io.SafeFileHandle;
                    writeToLog("Unusual name " + io);
                }
                AddToName(s);
            } else
            {
                origname = "" + stream;
                writeToLog("Non file stream  " + stream.GetType());
            }
        }

        private void writeToLog(string format, params object[] args)
        {
            if (format.Contains("ERROR"))
                HostSystem.writeToLog("ACF: " + this + " " + format, args);
        }

        public override string ToString()
        {
            AddToName("" + UnderlyingStream);
            string us = origname;
            if (IsClosed)
            {
                us = "!" + us;
            }
            return us;
        }

        public override void Close()
        {
            Close0();
        }

        public void Close0()
        {
            if (UnderlyingStreamSetNull)
            {
                IsClosed = true; 
                return;
            }
            try
            {
                UnderlyingStream.Close();
                IsClosed = true;
            }
            catch (Exception e)
            {
                writeToLog("ERROR Autoclosed: " + e);
                throw e;
            }
            this.UnderlyingStreamSetNull = true;
        }

        public void AddToName(string name)
        {
            if (origname.Contains(name))
            {
                return;
            }
            origname += " " + name;
        }

        #region Overrides of Stream

        public override bool CanTimeout
        {
            get
            {
                return UnderlyingStream.CanTimeout;
            }
        }

        protected override void Dispose(bool disposing)
        {
            if (!UnderlyingStreamSetNull)
            {
                UnderlyingStream.Dispose();
                UnderlyingStreamSetNull = true;
            }
            base.Dispose(disposing);
            IsClosed = true;
        }

        /// <summary>
        /// When overridden in a derived class, clears all buffers for this stream and causes any buffered data to be written to the underlying device.
        /// </summary>
        /// <exception cref="T:System.IO.IOException">An I/O error occurs. 
        ///                 </exception><filterpriority>2</filterpriority>
        public override void Flush()
        {
            UnderlyingStream.Flush();
        }

        /// <summary>
        /// When overridden in a derived class, sets the position within the current stream.
        /// </summary>
        /// <returns>
        /// The new position within the current stream.
        /// </returns>
        /// <param name="offset">A byte offset relative to the <paramref name="origin"/> parameter. 
        ///                 </param><param name="origin">A value of type <see cref="T:System.IO.SeekOrigin"/> indicating the reference point used to obtain the new position. 
        ///                 </param><exception cref="T:System.IO.IOException">An I/O error occurs. 
        ///                 </exception><exception cref="T:System.NotSupportedException">The stream does not support seeking, such as if the stream is constructed from a pipe or console output. 
        ///                 </exception><exception cref="T:System.ObjectDisposedException">Methods were called after the stream was closed. 
        ///                 </exception><filterpriority>1</filterpriority>
        public override long Seek(long offset, SeekOrigin origin)
        {
            var l = UnderlyingStream.Seek(offset, origin);
            PostRead(l);
            return l;
        }

        /// <summary>
        /// When overridden in a derived class, sets the length of the current stream.
        /// </summary>
        /// <param name="value">The desired length of the current stream in bytes. 
        ///                 </param><exception cref="T:System.IO.IOException">An I/O error occurs. 
        ///                 </exception><exception cref="T:System.NotSupportedException">The stream does not support both writing and seeking, such as if the stream is constructed from a pipe or console output. 
        ///                 </exception><exception cref="T:System.ObjectDisposedException">Methods were called after the stream was closed. 
        ///                 </exception><filterpriority>2</filterpriority>
        public override void SetLength(long value)
        {
            UnderlyingStream.SetLength(value);
            //if (Position >= Length) IsCloseable = true;
        }

        /// <summary>
        /// When overridden in a derived class, reads a sequence of bytes from the current stream and advances the position within the stream by the number of bytes read.
        /// </summary>
        /// <returns>
        /// The total number of bytes read into the buffer. This can be less than the number of bytes requested if that many bytes are not currently available, or zero (0) if the end of the stream has been reached.
        /// </returns>
        /// <param name="buffer">An array of bytes. When this method returns, the buffer contains the specified byte array with the values between <paramref name="offset"/> and (<paramref name="offset"/> + <paramref name="count"/> - 1) replaced by the bytes read from the current source. 
        ///                 </param><param name="offset">The zero-based byte offset in <paramref name="buffer"/> at which to begin storing the data read from the current stream. 
        ///                 </param><param name="count">The maximum number of bytes to be read from the current stream. 
        ///                 </param><exception cref="T:System.ArgumentException">The sum of <paramref name="offset"/> and <paramref name="count"/> is larger than the buffer length. 
        ///                 </exception><exception cref="T:System.ArgumentNullException"><paramref name="buffer"/> is null. 
        ///                 </exception><exception cref="T:System.ArgumentOutOfRangeException"><paramref name="offset"/> or <paramref name="count"/> is negative. 
        ///                 </exception><exception cref="T:System.IO.IOException">An I/O error occurs. 
        ///                 </exception><exception cref="T:System.NotSupportedException">The stream does not support reading. 
        ///                 </exception><exception cref="T:System.ObjectDisposedException">Methods were called after the stream was closed. 
        ///                 </exception><filterpriority>1</filterpriority>
        public override int Read(byte[] buffer, int offset, int count)
        {
            bool cr = CanRead;
            bool ic = IsClosed;
            if (ic && !cr)
            {
                return 0;
            }
            int r = UnderlyingStream.Read(buffer, offset, count);
            PostRead(r);
            return r;
        }

        private void PostRead(long r)
        {
            if (Position >= Length)
            {
                IsCloseable = true;
                if (r == 0)
                {
                    if (!IsClosed)
                    {
                        writeToLog("Autoclosing {0}>={1} r={2} ", Position , Length, r);
                        Close0();
                    }
                }
            }
        }


        /// <summary>
        /// When overridden in a derived class, writes a sequence of bytes to the current stream and advances the current position within this stream by the number of bytes written.
        /// </summary>
        /// <param name="buffer">An array of bytes. This method copies <paramref name="count"/> bytes from <paramref name="buffer"/> to the current stream. 
        ///                 </param><param name="offset">The zero-based byte offset in <paramref name="buffer"/> at which to begin copying bytes to the current stream. 
        ///                 </param><param name="count">The number of bytes to be written to the current stream. 
        ///                 </param><exception cref="T:System.ArgumentException">The sum of <paramref name="offset"/> and <paramref name="count"/> is greater than the buffer length. 
        ///                 </exception><exception cref="T:System.ArgumentNullException"><paramref name="buffer"/> is null. 
        ///                 </exception><exception cref="T:System.ArgumentOutOfRangeException"><paramref name="offset"/> or <paramref name="count"/> is negative. 
        ///                 </exception><exception cref="T:System.IO.IOException">An I/O error occurs. 
        ///                 </exception><exception cref="T:System.NotSupportedException">The stream does not support writing. 
        ///                 </exception><exception cref="T:System.ObjectDisposedException">Methods were called after the stream was closed. 
        ///                 </exception><filterpriority>1</filterpriority>
        public override void Write(byte[] buffer, int offset, int count)
        {
            UnderlyingStream.Write(buffer, offset, count);
        }

        /// <summary>
        /// When overridden in a derived class, gets a value indicating whether the current stream supports reading.
        /// </summary>
        /// <returns>
        /// true if the stream supports reading; otherwise, false.
        /// </returns>
        /// <filterpriority>1</filterpriority>
        public override bool CanRead
        {
            get { return UnderlyingStream.CanRead; }
        }

        /// <summary>
        /// When overridden in a derived class, gets a value indicating whether the current stream supports seeking.
        /// </summary>
        /// <returns>
        /// true if the stream supports seeking; otherwise, false.
        /// </returns>
        /// <filterpriority>1</filterpriority>
        public override bool CanSeek
        {
            get { return UnderlyingStream.CanSeek; }
        }

        /// <summary>
        /// When overridden in a derived class, gets a value indicating whether the current stream supports writing.
        /// </summary>
        /// <returns>
        /// true if the stream supports writing; otherwise, false.
        /// </returns>
        /// <filterpriority>1</filterpriority>
        public override bool CanWrite
        {
            get { return UnderlyingStream.CanWrite; }
        }

        /// <summary>
        /// When overridden in a derived class, gets the length in bytes of the stream.
        /// </summary>
        /// <returns>
        /// A long value representing the length of the stream in bytes.
        /// </returns>
        /// <exception cref="T:System.NotSupportedException">A class derived from Stream does not support seeking. 
        ///                 </exception><exception cref="T:System.ObjectDisposedException">Methods were called after the stream was closed. 
        ///                 </exception><filterpriority>1</filterpriority>
        public override long Length
        {
            get
            {
                if (IsClosed)
                {
                    return 0;
                }                
                return UnderlyingStream.Length;
            }
        }

        /// <summary>
        /// When overridden in a derived class, gets or sets the position within the current stream.
        /// </summary>
        /// <returns>
        /// The current position within the stream.
        /// </returns>
        /// <exception cref="T:System.IO.IOException">An I/O error occurs. 
        ///                 </exception><exception cref="T:System.NotSupportedException">The stream does not support seeking. 
        ///                 </exception><exception cref="T:System.ObjectDisposedException">Methods were called after the stream was closed. 
        ///                 </exception><filterpriority>1</filterpriority>
        public override long Position
        {
            get
            {
                if (IsClosed)
                {
                    return Length;
                }
                return UnderlyingStream.Position;
            }
            set { UnderlyingStream.Position = value; }
        }

        #endregion
    }
}