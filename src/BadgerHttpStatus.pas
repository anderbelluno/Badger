unit BadgerHttpStatus;

interface

const
  HTTP_CONTINUE = 100;
  HTTP_SWITCHING_PROTOCOLS = 101;
  HTTP_PROCESSING = 102;

  HTTP_OK = 200;
  HTTP_CREATED = 201;
  HTTP_ACCEPTED = 202;
  HTTP_NON_AUTHORITATIVE_INFORMATION = 203;
  HTTP_NO_CONTENT = 204;
  HTTP_RESET_CONTENT = 205;
  HTTP_PARTIAL_CONTENT = 206;
  HTTP_MULTI_STATUS = 207;
  HTTP_ALREADY_REPORTED = 208;
  HTTP_IM_USED = 226;

  HTTP_MULTIPLE_CHOICES = 300;
  HTTP_MOVED_PERMANENTLY = 301;
  HTTP_FOUND = 302;
  HTTP_SEE_OTHER = 303;
  HTTP_NOT_MODIFIED = 304;
  HTTP_USE_PROXY = 305;
  HTTP_TEMPORARY_REDIRECT = 307;
  HTTP_PERMANENT_REDIRECT = 308;

  HTTP_BAD_REQUEST = 400;
  HTTP_UNAUTHORIZED = 401;
  HTTP_PAYMENT_REQUIRED = 402;
  HTTP_FORBIDDEN = 403;
  HTTP_NOT_FOUND = 404;
  HTTP_METHOD_NOT_ALLOWED = 405;
  HTTP_NOT_ACCEPTABLE = 406;
  HTTP_PROXY_AUTHENTICATION_REQUIRED = 407;
  HTTP_REQUEST_TIMEOUT = 408;
  HTTP_CONFLICT = 409;
  HTTP_GONE = 410;
  HTTP_LENGTH_REQUIRED = 411;
  HTTP_PRECONDITION_FAILED = 412;
  HTTP_PAYLOAD_TOO_LARGE = 413;
  HTTP_REQUEST_URI_TOO_LONG = 414;
  HTTP_UNSUPPORTED_MEDIA_TYPE = 415;
  HTTP_REQUESTED_RANGE_NOT_SATISFIABLE = 416;
  HTTP_EXPECTATION_FAILED = 417;
  HTTP_IM_A_TEAPOT = 418;
  HTTP_MISDIRECTED_REQUEST = 421;
  HTTP_UNPROCESSABLE_ENTITY = 422;
  HTTP_LOCKED = 423;
  HTTP_FAILED_DEPENDENCY = 424;
  HTTP_UPGRADE_REQUIRED = 426;
  HTTP_PRECONDITION_REQUIRED = 428;
  HTTP_TOO_MANY_REQUESTS = 429;
  HTTP_REQUEST_HEADER_FIELDS_TOO_LARGE = 431;
  HTTP_CONNECTION_CLOSED_WITHOUT_RESPONSE = 444;
  HTTP_UNAVAILABLE_FOR_LEGAL_REASONS = 451;
  HTTP_CLIENT_CLOSED_REQUEST = 499;

  HTTP_INTERNAL_SERVER_ERROR = 500;
  HTTP_NOT_IMPLEMENTED = 501;
  HTTP_BAD_GATEWAY = 502;
  HTTP_SERVICE_UNAVAILABLE = 503;
  HTTP_GATEWAY_TIMEOUT = 504;
  HTTP_VERSION_NOT_SUPPORTED = 505;
  HTTP_VARIANT_ALSO_NEGOTIATES = 506;
  HTTP_INSUFFICIENT_STORAGE = 507;
  HTTP_LOOP_DETECTED = 508;
  HTTP_NOT_EXTENDED = 510;
  HTTP_NETWORK_AUTHENTICATION_REQUIRED = 511;
  HTTP_NETWORK_CONNECTION_TIMEOUT_ERROR = 599;

  //Content-Type
  APPLICATION_OCTET_STREAM = 'application/octet-stream';
  APPLICATION_JSON = 'application/json';
  TEXT_PLAIN = 'text/plain';

  type
  THTTPStatus = class
  public
    class function GetStatusText(StatusCode: Integer): string;
  end;

implementation

{ THTTPStatus }

class function THTTPStatus.GetStatusText(StatusCode: Integer): string;
begin
  case StatusCode of
    HTTP_CONTINUE: Result := 'Continue';
    HTTP_SWITCHING_PROTOCOLS: Result := 'Switching Protocols';
    HTTP_PROCESSING: Result := 'Processing';

    HTTP_OK: Result := 'OK';
    HTTP_CREATED: Result := 'Created';
    HTTP_ACCEPTED: Result := 'Accepted';
    HTTP_NON_AUTHORITATIVE_INFORMATION: Result := 'Non-authoritative Information';
    HTTP_NO_CONTENT: Result := 'No Content';
    HTTP_RESET_CONTENT: Result := 'Reset Content';
    HTTP_PARTIAL_CONTENT: Result := 'Partial Content';
    HTTP_MULTI_STATUS: Result := 'Multi-Status';
    HTTP_ALREADY_REPORTED: Result := 'Already Reported';
    HTTP_IM_USED: Result := 'IM Used';

    HTTP_MULTIPLE_CHOICES: Result := 'Multiple Choices';
    HTTP_MOVED_PERMANENTLY: Result := 'Moved Permanently';
    HTTP_FOUND: Result := 'Found';
    HTTP_SEE_OTHER: Result := 'See Other';
    HTTP_NOT_MODIFIED: Result := 'Not Modified';
    HTTP_USE_PROXY: Result := 'Use Proxy';
    HTTP_TEMPORARY_REDIRECT: Result := 'Temporary Redirect';
    HTTP_PERMANENT_REDIRECT: Result := 'Permanent Redirect';

    HTTP_BAD_REQUEST: Result := 'Bad Request';
    HTTP_UNAUTHORIZED: Result := 'Unauthorized';
    HTTP_PAYMENT_REQUIRED: Result := 'Payment Required';
    HTTP_FORBIDDEN: Result := 'Forbidden';
    HTTP_NOT_FOUND: Result := 'Not Found';
    HTTP_METHOD_NOT_ALLOWED: Result := 'Method Not Allowed';
    HTTP_NOT_ACCEPTABLE: Result := 'Not Acceptable';
    HTTP_PROXY_AUTHENTICATION_REQUIRED: Result := 'Proxy Authentication Required';
    HTTP_REQUEST_TIMEOUT: Result := 'Request Timeout';
    HTTP_CONFLICT: Result := 'Conflict';
    HTTP_GONE: Result := 'Gone';
    HTTP_LENGTH_REQUIRED: Result := 'Length Required';
    HTTP_PRECONDITION_FAILED: Result := 'Precondition Failed';
    HTTP_PAYLOAD_TOO_LARGE: Result := 'Payload Too Large';
    HTTP_REQUEST_URI_TOO_LONG: Result := 'Request-URI Too Long';
    HTTP_UNSUPPORTED_MEDIA_TYPE: Result := 'Unsupported Media Type';
    HTTP_REQUESTED_RANGE_NOT_SATISFIABLE: Result := 'Requested Range Not Satisfiable';
    HTTP_EXPECTATION_FAILED: Result := 'Expectation Failed';
    HTTP_IM_A_TEAPOT: Result := 'I''m a teapot';
    HTTP_MISDIRECTED_REQUEST: Result := 'Misdirected Request';
    HTTP_UNPROCESSABLE_ENTITY: Result := 'Unprocessable Entity';
    HTTP_LOCKED: Result := 'Locked';
    HTTP_FAILED_DEPENDENCY: Result := 'Failed Dependency';
    HTTP_UPGRADE_REQUIRED: Result := 'Upgrade Required';
    HTTP_PRECONDITION_REQUIRED: Result := 'Precondition Required';
    HTTP_TOO_MANY_REQUESTS: Result := 'Too Many Requests';
    HTTP_REQUEST_HEADER_FIELDS_TOO_LARGE: Result := 'Request Header Fields Too Large';
    HTTP_CONNECTION_CLOSED_WITHOUT_RESPONSE: Result := 'Connection Closed Without Response';
    HTTP_UNAVAILABLE_FOR_LEGAL_REASONS: Result := 'Unavailable For Legal Reasons';
    HTTP_CLIENT_CLOSED_REQUEST: Result := 'Client Closed Request';

    HTTP_INTERNAL_SERVER_ERROR: Result := 'Internal Server Error';
    HTTP_NOT_IMPLEMENTED: Result := 'Not Implemented';
    HTTP_BAD_GATEWAY: Result := 'Bad Gateway';
    HTTP_SERVICE_UNAVAILABLE: Result := 'Service Unavailable';
    HTTP_GATEWAY_TIMEOUT: Result := 'Gateway Timeout';
    HTTP_VERSION_NOT_SUPPORTED: Result := 'HTTP Version Not Supported';
    HTTP_VARIANT_ALSO_NEGOTIATES: Result := 'Variant Also Negotiates';
    HTTP_INSUFFICIENT_STORAGE: Result := 'Insufficient Storage';
    HTTP_LOOP_DETECTED: Result := 'Loop Detected';
    HTTP_NOT_EXTENDED: Result := 'Not Extended';
    HTTP_NETWORK_AUTHENTICATION_REQUIRED: Result := 'Network Authentication Required';
    HTTP_NETWORK_CONNECTION_TIMEOUT_ERROR: Result := 'Network Connection Timeout Error';
  else
    Result := 'Unknown';
  end;
end;

end.
