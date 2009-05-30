using System;

namespace HttpServer
{
    /// <summary>
    /// Contains a signature pattern (for matching against incoming
    /// requests) and a callback for handling the request
    /// </summary>
    [System.Diagnostics.DebuggerDisplay("{Signature}")]
    public sealed class HttpRequestHandler : IEquatable<HttpRequestHandler>
    {
        /// <summary>Signature pattern to match against incoming requests</summary>
        public HttpRequestSignature Signature;
        /// <summary>Callback for handling requests that match the signature</summary>
        public HttpRequestCallback Callback;
        /// <summary>If true, the IHttpResponse will be sent to the client after
        /// the callback completes. Otherwise, the connection will be left open 
        /// and the user is responsible for closing the connection later</summary>
        public bool SendResponseAfterCallback;

        /// <summary>
        /// Default constructor
        /// </summary>
        /// <param name="signature">Signature pattern for matching against incoming requests</param>
        /// <param name="callback">Callback for handling the request</param>
        /// <param name="sendResponseAfterCallback">If true, the IHttpResponse will be sent 
        /// to the client after the callback completes. Otherwise, the connection will be left
        /// open and the user is responsible for closing the connection later</param>
        public HttpRequestHandler(HttpRequestSignature signature, HttpRequestCallback callback, bool sendResponseAfterCallback)
        {
            Signature = signature;
            Callback = callback;
            SendResponseAfterCallback = sendResponseAfterCallback;
        }

        /// <summary>
        /// Equality comparison
        /// </summary>
        /// <param name="obj">Object to compare against for equality</param>
        /// <returns>True if the given object is equal to this object, otherwise false</returns>
        public override bool Equals(object obj)
        {
            return (obj is HttpRequestHandler) ? this.Signature == ((HttpRequestHandler)obj).Signature : false;
        }

        /// <summary>
        /// Equality comparison
        /// </summary>
        /// <param name="handler">Object to compare against for equality</param>
        /// <returns>True if the given object is equal to this object, otherwise false</returns>
        public bool Equals(HttpRequestHandler handler)
        {
            return handler != null && this.Signature == handler.Signature;
        }

        /// <summary>
        /// Returns the hash code for the signature in this handler
        /// </summary>
        /// <returns>The hash code for the signature in this handler</returns>
        public override int GetHashCode()
        {
            return Signature.GetHashCode();
        }

        public static bool operator==(HttpRequestHandler left, HttpRequestHandler right)
        {
            if ((object)left == null)
                return (object)right == null;
            else if ((object)right == null)
                return false;
            else
                return left.Signature == right.Signature;
        }

        public static bool operator !=(HttpRequestHandler left, HttpRequestHandler right)
        {
            return !(left == right);
        }
    }
}
