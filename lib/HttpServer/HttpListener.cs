using System;
using System.Net;
using System.Net.Sockets;
using System.Threading;
using System.Security.Authentication;
using System.Security.Cryptography.X509Certificates;

#if VISUAL_STUDIO
using ReaderWriterLockImpl = System.Threading.ReaderWriterLockSlim;
#else
using ReaderWriterLockImpl = HttpServer.ReaderWriterLockSlim;
#endif

namespace HttpServer
{
    /// <summary>
    /// Delegate for handling incoming HTTP requests
    /// </summary>
    /// <param name="context">Client context</param>
    /// <param name="request">HTTP request</param>
    /// <param name="response">HTTP response</param>
    public delegate void HttpRequestCallback(IHttpClientContext context, IHttpRequest request, IHttpResponse response);

    /// <summary>
    /// New implementation of the HTTP listener.
    /// </summary>
    /// <remarks>
    /// Use the <c>Create</c> methods to create a default listener.
    /// </remarks>
    public class HttpListener : HttpListenerBase
    {
        /// <summary>
        /// A client have been accepted, but not handled, by the listener.
        /// </summary>
        public event EventHandler<ClientAcceptedEventArgs> Accepted = delegate{};

        HttpRequestHandler[] _requestHandlers = new HttpRequestHandler[0];
        HttpRequestHandler _notFoundHandler;
        RequestQueue _requestQueue;
        int _backLog = 10;
        ReaderWriterLockImpl rwHandlersLock = new ReaderWriterLockImpl();

        #region Properties

        /// <summary>
        /// Number of connections that can wait to be accepted by the server.
        /// </summary>
        /// <remarks>Default is 10.</remarks>
        public int BackLog
        {
            get { return _backLog; }
            set { _backLog = value; }
        }

        /// <summary>
        /// Gets or sets maximum number of allowed simultaneous requests.
        /// </summary>
        /// <remarks>
        /// <para>
        /// This property is useful in busy systems. The HTTP server
        /// will start queuing new requests if this limit is hit, instead
        /// of trying to process all incoming requests directly.
        /// </para>
        /// <para>
        /// The default number if allowed simultaneous requests are 10.
        /// </para>
        /// </remarks>
        public int MaxRequestCount
        {
            get { return _requestQueue.MaxRequestCount; }
            set { _requestQueue.MaxRequestCount = value; }
        }

        /// <summary>
        /// Gets or sets maximum number of requests queuing to be handled.
        /// </summary>
        /// <remarks>
        /// <para>
        /// The WebServer will start turning requests away if response code
        /// <see cref="HttpStatusCode.ServiceUnavailable"/> to indicate that the server
        /// is too busy to be able to handle the request.
        /// </para>
        /// </remarks>
        public int MaxQueueSize
        {
            get { return _requestQueue.MaxQueueSize; }
            set { _requestQueue.MaxQueueSize = value; }
        }

        #endregion Properties

        #region Constructors / Create

        /// <summary>
        /// Initializes a new instance of the <see cref="HttpListener"/> class.
        /// </summary>
        /// <param name="address">IP Address to accept connections on</param>
        /// <param name="port">TCP Port to listen on, default HTTP port is 80.</param>
        /// <param name="factory">Factory used to create <see cref="IHttpClientContext"/>es.</param>
        /// <exception cref="ArgumentNullException"><c>address</c> is null.</exception>
        /// <exception cref="ArgumentException">Port must be a positive number.</exception>
        HttpListener(IPAddress address, int port, IHttpContextFactory factory)
            : base(address, port, factory)
        {
            Init();
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="HttpListener"/> class.
        /// </summary>
        /// <param name="address">The address.</param>
        /// <param name="port">The port.</param>
        /// <param name="factory">The factory.</param>
        /// <param name="certificate">The certificate.</param>
        HttpListener(IPAddress address, int port, IHttpContextFactory factory, X509Certificate certificate)
            : base(address, port, factory, certificate)
        {
            Init();
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="HttpListener"/> class.
        /// </summary>
        /// <param name="address">The address.</param>
        /// <param name="port">The port.</param>
        /// <param name="factory">The factory.</param>
        /// <param name="certificate">The certificate.</param>
        /// <param name="protocol">The protocol.</param>
        /// <param name="requireClientCerts">True if client SSL certificates are required, otherwise false</param>
        HttpListener(IPAddress address, int port, IHttpContextFactory factory, X509Certificate certificate, SslProtocols protocol,
            bool requireClientCerts)
            : base(address, port, factory, certificate, protocol, requireClientCerts)
        {
            Init();
        }

        /// <summary>
        /// Creates a new <see cref="HttpListener"/> instance with default factories.
        /// </summary>
        /// <param name="log">Logging engine for the server. Use NullLogWriter.Instance to disable</param>
        /// <param name="address">Address that the listener should accept connections on.</param>
        /// <param name="port">Port that listener should accept connections on.</param>
		/// <returns>Created HTTP listener.</returns>
        public static HttpListener Create(ILogWriter log, IPAddress address, int port)
        {
            RequestParserFactory requestFactory = new RequestParserFactory();
            HttpContextFactory factory = new HttpContextFactory(log, 16384, requestFactory, null);
            HttpListener listener = new HttpListener(address, port, factory);
            listener._logWriter = log;
            return listener;
        }

        /// <summary>
        /// Creates a new <see cref="HttpListener"/> instance with default factories.
        /// </summary>
        /// <param name="log">Logging engine for the server. Use NullLogWriter.Instance to disable</param>
        /// <param name="address">Address that the listener should accept connections on.</param>
        /// <param name="port">Port that listener should accept connections on.</param>
        /// <param name="certificate">Certificate to use</param>
		/// <returns>Created HTTP listener.</returns>
        public static HttpListener Create(ILogWriter log, IPAddress address, int port, X509Certificate certificate)
        {
            RequestParserFactory requestFactory = new RequestParserFactory();
            HttpContextFactory factory = new HttpContextFactory(log, 16384, requestFactory, null);
            HttpListener listener = new HttpListener(address, port, factory, certificate);
            listener._logWriter = log;
            return listener;
        }

        /// <summary>
        /// Creates a new <see cref="HttpListener"/> instance with default factories.
        /// </summary>
        /// <param name="log">Logging engine for the server. Use NullLogWriter.Instance to disable</param>
        /// <param name="address">Address that the listener should accept connections on.</param>
        /// <param name="port">Port that listener should accept connections on.</param>
        /// <param name="certificate">Certificate to use</param>
        /// <param name="rootCA">Root certificate that incoming client certificates have been signed with</param>
        /// <param name="protocol">which HTTPS protocol to use, default is TLS.</param>
        /// <returns>Created HTTP listener.</returns>
        /// <param name="requireClientCerts">True if client SSL certificates are required, otherwise false</param>
        public static HttpListener Create(ILogWriter log, IPAddress address, int port, X509Certificate certificate,
            X509Certificate rootCA, SslProtocols protocol, bool requireClientCerts)
        {
            RequestParserFactory requestFactory = new RequestParserFactory();
            HttpContextFactory factory = new HttpContextFactory(log, 16384, requestFactory, rootCA);
            HttpListener listener = new HttpListener(address, port, factory, certificate, protocol, requireClientCerts);
            listener._logWriter = log;
            return listener;
        }

        #endregion Constructors / Create

        public override void Stop()
        {
            base.Stop();
            _requestQueue.Stop();
        }

        /// <summary>
        /// Add a request handler
        /// </summary>
        /// <param name="method">HTTP verb to match, or null to skip verb matching</param>
        /// <param name="contentType">Content-Type header to match, or null to skip Content-Type matching</param>
        /// <param name="path">Request URI path regular expression to match, or null to skip URI path matching</param>
        /// <param name="callback">Callback to fire when an incoming request matches the given pattern</param>
        /// <remarks>Using this overload, the response will automatically be sent when the callback completes</remarks>
        public void AddHandler(string method, string contentType, string path, HttpRequestCallback callback)
        {
            HttpRequestSignature signature = new HttpRequestSignature(method, contentType, path);
            AddHandler(new HttpRequestHandler(signature, callback, true));
        }

        /// <summary>
        /// Add a request handler
        /// </summary>
        /// <param name="method">HTTP verb to match, or null to skip verb matching</param>
        /// <param name="contentType">Content-Type header to match, or null to skip Content-Type matching</param>
        /// <param name="path">Request URI path regular expression to match, or null to skip URI path matching</param>
        /// <param name="sendResponseAfterCallback">If true, the IHttpResponse will be sent to the client after
        /// the callback completes. Otherwise, the connection will be left open and the user is responsible for
        /// closing the connection later</param>
        /// <param name="callback">Callback to fire when an incoming request matches the given pattern</param>
        public void AddHandler(string method, string contentType, string path, bool sendResponseAfterCallback, HttpRequestCallback callback)
        {
            HttpRequestSignature signature = new HttpRequestSignature(method, contentType, path);
            AddHandler(new HttpRequestHandler(signature, callback, sendResponseAfterCallback));
        }

        /// <summary>
        /// Add a request handler
        /// </summary>
        /// <param name="handler">Request handler to add</param>
        public void AddHandler(HttpRequestHandler handler)
        {
            rwHandlersLock.EnterWriteLock();

            try
            {
                HttpRequestHandler[] newHandlers = new HttpRequestHandler[_requestHandlers.Length + 1];

                for (int i = 0; i < _requestHandlers.Length; i++)
                    newHandlers[i] = _requestHandlers[i];
                newHandlers[_requestHandlers.Length] = handler;

                _requestHandlers = newHandlers;
            }
            finally { rwHandlersLock.ExitWriteLock(); }
        }

        /// <summary>
        /// Remove a request handler
        /// </summary>
        /// <param name="handler">Request handler to remove</param>
        public void RemoveHandler(HttpRequestHandler handler)
        {
            rwHandlersLock.EnterWriteLock();

            try
            {
                HttpRequestHandler[] newHandlers = new HttpRequestHandler[_requestHandlers.Length - 1];

                int j = 0;
                for (int i = 0; i < _requestHandlers.Length; i++)
                    if (!_requestHandlers[i].Signature.ExactlyEquals(handler.Signature))
                        newHandlers[j++] = handler;

                _requestHandlers = newHandlers;
            }
            finally { rwHandlersLock.ExitWriteLock(); }
        }

        /// <summary>
        /// Set a callback to override the default 404 (Not Found) response
        /// </summary>
        /// <param name="callback">Callback that will be fired when an unhandled
        /// request is received, or null to reset to the default handler</param>
        public void Set404Handler(HttpRequestCallback callback)
        {
            _notFoundHandler = new HttpRequestHandler(null, callback, true);
        }

        protected void Init()
        {
            RequestReceived += RequestHandler;
            _notFoundHandler = new HttpRequestHandler(null, Default404Handler, true);

            if (_requestQueue == null)
                _requestQueue = new RequestQueue(ProcessRequestWrapper);
            _requestQueue.Start();
        }

        /// <summary>
        /// Can be used to create filtering of new connections.
        /// </summary>
        /// <param name="socket">Accepted socket</param>
        /// <returns>
        /// true if connection can be accepted; otherwise false.
        /// </returns>
        protected override bool OnAcceptingSocket(Socket socket)
        {
            ClientAcceptedEventArgs args = new ClientAcceptedEventArgs(socket);
            Accepted(this, args);
            return !args.Revoked;
        }

        void RequestHandler(object sender, RequestEventArgs e)
        {
            IHttpClientContext context = (IHttpClientContext)sender;
            IHttpRequest request = e.Request;
            if (_requestQueue.ShouldQueue)
            {
                _requestQueue.Enqueue(context, request);
                return;
            }

            ProcessRequestWrapper(context, request);

            // no need to lock, if all threads are busy,
            // someone is bound to trigger the thread correctly =)
            _requestQueue.Trigger();
        }

        void ProcessRequestWrapper(IHttpClientContext context, IHttpRequest request)
        {
            _requestQueue.CurrentRequestCount += 1;
            ProcessRequest(context, request);
            _requestQueue.CurrentRequestCount -= 1;
        }

        void ProcessRequest(IHttpClientContext context, IHttpRequest request)
        {
            LogWriter.Write(this, LogPrio.Trace, "Processing request...");

            IHttpResponse response = request.CreateResponse(context);

            // Load cookies if they exist
            RequestCookies cookies = request.Headers["cookie"] != null
                                         ? new RequestCookies(request.Headers["cookie"])
                                         : new RequestCookies(String.Empty);
            request.SetCookies(cookies);

            // Create a request signature
            HttpRequestSignature signature = new HttpRequestSignature(request);

            // Look for a signature match in our handlers
            HttpRequestHandler foundHandler = null;

            bool doLock = !rwHandlersLock.IsReadLockHeld;
            if (doLock) rwHandlersLock.EnterReadLock();

            try
            {
                for (int i = 0; i < _requestHandlers.Length; i++)
                {
                    HttpRequestHandler handler = _requestHandlers[i];

                    if (signature == handler.Signature)
                    {
                        foundHandler = handler;
                        break;
                    }
                }
            }
            finally { if (doLock) rwHandlersLock.ExitReadLock(); }

            if (foundHandler != null)
                FireRequestCallback(context, request, response, foundHandler);
            else
                FireRequestCallback(context, request, response, _notFoundHandler);

            LogWriter.Write(this, LogPrio.Trace, "...done processing request.");
        }

        void FireRequestCallback(IHttpClientContext client, IHttpRequest request, IHttpResponse response, HttpRequestHandler handler)
        {
            try
            {
                handler.Callback(client, request, response);
            }
            catch (Exception ex)
            {
                LogWriter.Write(this, LogPrio.Error, "Exception in HTTP handler: " + ex);
                response.Status = HttpStatusCode.InternalServerError;
                response.Send();
            }

            if (handler.SendResponseAfterCallback && !response.Sent)
            {
                try { response.Send(); }
                catch (Exception ex)
                {
                    LogWriter.Write(this, LogPrio.Error, String.Format("Failed to send HTTP response for request to {0}: {1}",
                        request.Uri, ex.Message));
                }
            }

            request.Clear();
        }

        void Default404Handler(IHttpClientContext client, IHttpRequest request, IHttpResponse response)
        {
            response.Status = HttpStatusCode.NotFound;
            string notFoundResponse = "<html><head><title>Page Not Found</title></head><body><h3>" + response.Reason + "</h3></body></html>";
            byte[] buffer = System.Text.Encoding.UTF8.GetBytes(notFoundResponse);
            response.Body.Write(buffer, 0, buffer.Length);
        }
    }
}
