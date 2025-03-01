unit BadgerHttpStatus;

interface

type
  THTTPStatus = class
  public
    // 1xx Informational
    const Continue = 100;
    const SwitchingProtocols = 101;
    const Processing = 102;

    // 2xx Success
    const OK = 200;
    const Created = 201;
    const Accepted = 202;
    const NonAuthoritativeInformation = 203;
    const NoContent = 204;
    const ResetContent = 205;
    const PartialContent = 206;
    const MultiStatus = 207;
    const AlreadyReported = 208;
    const IMUsed = 226;

    // 3xx Redirection
    const MultipleChoices = 300;
    const MovedPermanently = 301;
    const Found = 302;
    const SeeOther = 303;
    const NotModified = 304;
    const UseProxy = 305;
    const TemporaryRedirect = 307;
    const PermanentRedirect = 308;

    // 4xx Client Error
    const BadRequest = 400;
    const Unauthorized = 401;
    const PaymentRequired = 402;
    const Forbidden = 403;
    const NotFound = 404;
    const MethodNotAllowed = 405;
    const NotAcceptable = 406;
    const ProxyAuthenticationRequired = 407;
    const RequestTimeout = 408;
    const Conflict = 409;
    const Gone = 410;
    const LengthRequired = 411;
    const PreconditionFailed = 412;
    const PayloadTooLarge = 413;
    const RequestURITooLong = 414;
    const UnsupportedMediaType = 415;
    const RequestedRangeNotSatisfiable = 416;
    const ExpectationFailed = 417;
    const ImATeapot = 418;
    const MisdirectedRequest = 421;
    const UnprocessableEntity = 422;
    const Locked = 423;
    const FailedDependency = 424;
    const UpgradeRequired = 426;
    const PreconditionRequired = 428;
    const TooManyRequests = 429;
    const RequestHeaderFieldsTooLarge = 431;
    const ConnectionClosedWithoutResponse = 444;
    const UnavailableForLegalReasons = 451;
    const ClientClosedRequest = 499;

    // 5xx Server Error
    const InternalServerError = 500;
    const NotImplemented = 501;
    const BadGateway = 502;
    const ServiceUnavailable = 503;
    const GatewayTimeout = 504;
    const HTTPVersionNotSupported = 505;
    const VariantAlsoNegotiates = 506;
    const InsufficientStorage = 507;
    const LoopDetected = 508;
    const NotExtended = 510;
    const NetworkAuthenticationRequired = 511;
    const NetworkConnectionTimeoutError = 599;

    class function GetStatusText(StatusCode: Integer): string;
  end;

implementation

{ THTTPStatus }

class function THTTPStatus.GetStatusText(StatusCode: Integer): string;
begin
  case StatusCode of
    // 1xx Informational
    Continue: Result := 'Continue';
    SwitchingProtocols: Result := 'Switching Protocols';
    Processing: Result := 'Processing';

    // 2xx Success
    OK: Result := 'OK';
    Created: Result := 'Created';
    Accepted: Result := 'Accepted';
    NonAuthoritativeInformation: Result := 'Non-authoritative Information';
    NoContent: Result := 'No Content';
    ResetContent: Result := 'Reset Content';
    PartialContent: Result := 'Partial Content';
    MultiStatus: Result := 'Multi-Status';
    AlreadyReported: Result := 'Already Reported';
    IMUsed: Result := 'IM Used';

    // 3xx Redirection
    MultipleChoices: Result := 'Multiple Choices';
    MovedPermanently: Result := 'Moved Permanently';
    Found: Result := 'Found';
    SeeOther: Result := 'See Other';
    NotModified: Result := 'Not Modified';
    UseProxy: Result := 'Use Proxy';
    TemporaryRedirect: Result := 'Temporary Redirect';
    PermanentRedirect: Result := 'Permanent Redirect';

    // 4xx Client Error
    BadRequest: Result := 'Bad Request';
    Unauthorized: Result := 'Unauthorized';
    PaymentRequired: Result := 'Payment Required';
    Forbidden: Result := 'Forbidden';
    NotFound: Result := 'Not Found';
    MethodNotAllowed: Result := 'Method Not Allowed';
    NotAcceptable: Result := 'Not Acceptable';
    ProxyAuthenticationRequired: Result := 'Proxy Authentication Required';
    RequestTimeout: Result := 'Request Timeout';
    Conflict: Result := 'Conflict';
    Gone: Result := 'Gone';
    LengthRequired: Result := 'Length Required';
    PreconditionFailed: Result := 'Precondition Failed';
    PayloadTooLarge: Result := 'Payload Too Large';
    RequestURITooLong: Result := 'Request-URI Too Long';
    UnsupportedMediaType: Result := 'Unsupported Media Type';
    RequestedRangeNotSatisfiable: Result := 'Requested Range Not Satisfiable';
    ExpectationFailed: Result := 'Expectation Failed';
    ImATeapot: Result := 'I''m a teapot';
    MisdirectedRequest: Result := 'Misdirected Request';
    UnprocessableEntity: Result := 'Unprocessable Entity';
    Locked: Result := 'Locked';
    FailedDependency: Result := 'Failed Dependency';
    UpgradeRequired: Result := 'Upgrade Required';
    PreconditionRequired: Result := 'Precondition Required';
    TooManyRequests: Result := 'Too Many Requests';
    RequestHeaderFieldsTooLarge: Result := 'Request Header Fields Too Large';
    ConnectionClosedWithoutResponse: Result := 'Connection Closed Without Response';
    UnavailableForLegalReasons: Result := 'Unavailable For Legal Reasons';
    ClientClosedRequest: Result := 'Client Closed Request';

    // 5xx Server Error
    InternalServerError: Result := 'Internal Server Error';
    NotImplemented: Result := 'Not Implemented';
    BadGateway: Result := 'Bad Gateway';
    ServiceUnavailable: Result := 'Service Unavailable';
    GatewayTimeout: Result := 'Gateway Timeout';
    HTTPVersionNotSupported: Result := 'HTTP Version Not Supported';
    VariantAlsoNegotiates: Result := 'Variant Also Negotiates';
    InsufficientStorage: Result := 'Insufficient Storage';
    LoopDetected: Result := 'Loop Detected';
    NotExtended: Result := 'Not Extended';
    NetworkAuthenticationRequired: Result := 'Network Authentication Required';
    NetworkConnectionTimeoutError: Result := 'Network Connection Timeout Error';

  else
    Result := 'Unknown';
  end;
end;

end.
