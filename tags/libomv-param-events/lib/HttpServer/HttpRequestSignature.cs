using System;
using System.Net;
using System.Text.RegularExpressions;

namespace HttpServer
{
    /// <summary>
    /// Used to match incoming HTTP requests against registered handlers.
    /// Matches based on any combination of HTTP Method, Content-Type header,
    /// and URL path. URL path matching supports the * wildcard character
    /// </summary>
    [System.Diagnostics.DebuggerDisplay("{method} {path} Content-Type: {contentType}")]
    public sealed class HttpRequestSignature : IEquatable<HttpRequestSignature>
    {
        private string method;
        private string contentType;
        private string path;

        /// <summary>
        /// Builds an HttpRequestSignature from passed in parameters
        /// </summary>
        /// <param name="method">HTTP method to match, or null to skip</param>
        /// <param name="contentType">HTTP Content-Type header to match, or null to skip</param>
        /// <param name="path">Regular expression to match against the relative URL path and query</param>
        public HttpRequestSignature(string method, string contentType, string path)
        {
            this.method = (method != null) ? method.ToUpperInvariant() : String.Empty;
            this.contentType = (contentType != null) ? contentType.ToLowerInvariant() : String.Empty;
            this.path = path ?? String.Empty;
        }

        /// <summary>
        /// Builds an HttpRequestSignature from an incoming request
        /// </summary>
        /// <param name="request">Incoming request to build the signature from</param>
        public HttpRequestSignature(IHttpRequest request)
        {
            this.method = request.Method.ToUpperInvariant();
            this.contentType = request.Headers.Get("content-type");
            this.contentType = (this.contentType != null) ? this.contentType.ToLowerInvariant() : String.Empty;
            this.path = request.Uri.PathAndQuery;
        }

        /// <summary>
        /// Test if two HTTP request signatures contain exactly the same data
        /// </summary>
        /// <param name="signature">Signature to test against</param>
        /// <returns>True if the contents of both signatures are identical, 
        /// otherwise false</returns>
        public bool ExactlyEquals(HttpRequestSignature signature)
        {
            return method.Equals(signature.method) &&
                contentType.Equals(signature.contentType) &&
                path.Equals(signature.path);
        }

        /// <summary>
        /// Does pattern matching to determine if an incoming HTTP request
        /// matches a given pattern. Equals can only be called on an incoming
        /// request; the pattern to match against is the parameter
        /// </summary>
        /// <param name="obj">The pattern to test against this request</param>
        /// <returns>True if the request matches the given pattern, otherwise
        /// false</returns>
        public override bool Equals(object obj)
        {
            return (obj is HttpRequestSignature) ? this == (HttpRequestSignature)obj : false;
        }

        /// <summary>
        /// Does pattern matching to determine if an incoming HTTP request
        /// matches a given pattern. Equals can only be called on an incoming
        /// request; the pattern to match against is the parameter
        /// </summary>
        /// <param name="pattern">The pattern to test against this request</param>
        /// <returns>True if the request matches the given pattern, otherwise
        /// false</returns>
        public bool Equals(HttpRequestSignature pattern)
        {
            return (pattern != null && this == pattern);
        }

        /// <summary>
        /// Returns the hash code for this signature
        /// </summary>
        /// <returns>Hash code for this signature</returns>
        public override int GetHashCode()
        {
            return method.GetHashCode() ^ contentType.GetHashCode() ^ path.GetHashCode();
        }

        /// <summary>
        /// Does pattern matching to determine if an incoming HTTP request
        /// matches a given pattern. The incoming request must be on the
        /// left-hand side, and the pattern to match against must be on the
        /// right-hand side
        /// </summary>
        /// <param name="request">The incoming HTTP request signature</param>
        /// <param name="pattern">The pattern to test against the incoming request</param>
        /// <returns>True if the request matches the given pattern, otherwise
        /// false</returns>
        public static bool operator ==(HttpRequestSignature request, HttpRequestSignature pattern)
        {
            return
                (String.IsNullOrEmpty(pattern.method) || request.method == pattern.method) &&
                (String.IsNullOrEmpty(pattern.contentType) || request.contentType == pattern.contentType) &&
                (String.IsNullOrEmpty(pattern.path) || Regex.IsMatch(request.path, pattern.path, RegexOptions.IgnoreCase | RegexOptions.Compiled));
        }

        /// <summary>
        /// Does pattern matching to determine if an incoming HTTP request
        /// matches a given pattern. The incoming request must be on the
        /// left-hand side, and the pattern to match against must be on the
        /// right-hand side
        /// </summary>
        /// <param name="request">The incoming HTTP request signature</param>
        /// <param name="pattern">The pattern to test against the incoming request</param>
        /// <returns>True if the request does not match the given pattern, otherwise
        /// false</returns>
        public static bool operator !=(HttpRequestSignature request, HttpRequestSignature pattern)
        {
            return !(request == pattern);
        }
    }
}
