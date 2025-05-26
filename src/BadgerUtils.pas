unit BadgerUtils;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes
  {$IFDEF WINDOWS}, Registry {$ENDIF};

type
  TBytes = array of Byte;

  TBadgerUtils = class(TObject)
  private
    FMimeList: TStringList;
    procedure GetMIMETableFromOS(const AMIMEList: TStringList);
    procedure BuildMimelist(aMimeList: TStringList);
  public
    constructor Create;
    destructor Destroy; override;
    function GetFileMIMEType(const AFileName: string): string;
  end;

function CustomEncodeBase64(const Input: string; URLSafe: Boolean): string;
function CustomDecodeBase64(const Input: string): string;
function BytesToRawString(const ABytes: TBytes): string;
function RawStringToBytes(const S: string): TBytes;

implementation

const
  Base64Alphabet: array [0 .. 63] of Char = (
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
    'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
    'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
    'w', 'x', 'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '/'
  );

{ TBadgerUtils }

constructor TBadgerUtils.Create;
begin
  inherited Create;
  FMimeList := TStringList.Create;
  FMimeList.Sorted := True;
  FMimeList.Duplicates := dupIgnore;
end;

destructor TBadgerUtils.Destroy;
begin
  FMimeList.Free;
  inherited;
end;

procedure TBadgerUtils.GetMIMETableFromOS(const AMIMEList: TStringList);
{$IFDEF WINDOWS}
var
  reg: TRegistry;
  KeyList: TStringList;
  I: Integer;
  S, LExt: string;
{$ENDIF}
begin
{$IFNDEF WINDOWS}
  Exit;
{$ELSE}
  AMIMEList.Sorted := False;
  reg := TRegistry.Create;
  try
    KeyList := TStringList.Create;
    try
      reg.RootKey := HKEY_CLASSES_ROOT;
      if reg.OpenKeyReadOnly('\') then
      begin
        reg.GetKeyNames(KeyList);
        reg.Closekey;
      end;
      for I := 0 to KeyList.Count - 1 do
      begin
        LExt := KeyList.Strings[I];
        if Copy(LExt, 1, 1) = '.' then
        begin
          if reg.OpenKeyReadOnly(LExt) then
          begin
            S := reg.ReadString('Content Type');
            if Length(S) > 0 then
              AMIMEList.Values[LowerCase(LExt)] := LowerCase(S);
            reg.Closekey;
          end;
        end;
      end;
      if reg.OpenKeyReadOnly('\MIME\Database\Content Type') then
      begin
        KeyList.Clear;
        reg.GetKeyNames(KeyList);
        reg.Closekey;
        for I := 0 to KeyList.Count - 1 do
        begin
          if reg.OpenKeyReadOnly('\MIME\Database\Content Type\' + KeyList[I]) then
          begin
            LExt := LowerCase(reg.ReadString('Extension'));
            if Length(LExt) > 0 then
            begin
              if LExt[1] <> '.' then
                LExt := '.' + LExt;
              AMIMEList.Values[LExt] := LowerCase(KeyList[I]);
            end;
            reg.Closekey;
          end;
        end;
      end;
    finally
      KeyList.Free;
    end;
  finally
    reg.Free;
    AMIMEList.Sort;
    AMIMEList.Sorted := True;
  end;
{$ENDIF}
end;

procedure TBadgerUtils.BuildMimelist(aMimeList: TStringList);
begin
  GetMIMETableFromOS(aMimeList);
  aMimeList.BeginUpdate;
  aMimeList.Add('.323=text/h323');
  aMimeList.Add('.3g2=video/3gpp2');
  aMimeList.Add('.3gp=video/3gpp');
  aMimeList.Add('.7z=application/x-7z-compressed');
  aMimeList.Add('.a=application/x-archive');
  aMimeList.Add('.aab=application/x-authorware-bin');
  aMimeList.Add('.aac=audio/aac');
  aMimeList.Add('.aam=application/x-authorware-map');
  aMimeList.Add('.aas=application/x-authorware-seg');
  aMimeList.Add('.abw=application/x-abiword');
  aMimeList.Add('.ace=application/x-ace-compressed');
  aMimeList.Add('.ai=application/postscript');
  aMimeList.Add('.aif=audio/x-aiff');
  aMimeList.Add('.aifc=audio/x-aiff');
  aMimeList.Add('.aiff=audio/x-aiff');
  aMimeList.Add('.alz=application/x-alz-compressed');
  aMimeList.Add('.ani=application/x-navi-animation');
  aMimeList.Add('.arc=application/x-freearc');
  aMimeList.Add('.arj=application/x-arj');
  aMimeList.Add('.art=image/x-jg');
  aMimeList.Add('.asf=application/vnd.ms-asf');
  aMimeList.Add('.asf=video/x-ms-asf');
  aMimeList.Add('.asm=text/x-asm');
  aMimeList.Add('.asx=video/x-ms-asf-plugin');
  aMimeList.Add('.asx=video/x-ms-asf');
  aMimeList.Add('.au=audio/basic');
  aMimeList.Add('.avi=video/x-msvideo');
  aMimeList.Add('.avif=image/avif');
  aMimeList.Add('.azw=application/vnd.amazon.ebook');
  aMimeList.Add('.bat=application/x-msdos-program');
  aMimeList.Add('.bcpio=application/x-bcpio');
  aMimeList.Add('.bin=application/octet-stream');
  aMimeList.Add('.bmp=image/bmp');
  aMimeList.Add('.boz=application/x-bzip2');
  aMimeList.Add('.bz=application/x-bzip');
  aMimeList.Add('.bz2=application/x-bzip2');
  aMimeList.Add('.c=text/x-csrc');
  aMimeList.Add('.c++=text/x-c++src');
  aMimeList.Add('.cab=application/vnd.ms-cab-compressed');
  aMimeList.Add('.cat=application/vnd.ms-pki.seccat');
  aMimeList.Add('.cc=text/x-c++src');
  aMimeList.Add('.ccn=application/x-cnc');
  aMimeList.Add('.cco=application/x-cocoa');
  aMimeList.Add('.cda=application/x-cdf');
  aMimeList.Add('.cdf=application/x-cdf');
  aMimeList.Add('.cdr=image/x-coreldraw');
  aMimeList.Add('.cdt=image/x-coreldrawtemplate');
  aMimeList.Add('.cer=application/x-x509-ca-cert');
  aMimeList.Add('.chm=application/vnd.ms-htmlhelp');
  aMimeList.Add('.chrt=application/vnd.kde.kchart');
  aMimeList.Add('.cil=application/vnd.ms-artgalry');
  aMimeList.Add('.class=application/java-vm');
  aMimeList.Add('.clp=application/x-msclip');
  aMimeList.Add('.com=application/x-msdos-program');
  aMimeList.Add('.cpio=application/x-cpio');
  aMimeList.Add('.cpp=text/x-c++src');
  aMimeList.Add('.cpt=application/mac-compactpro');
  aMimeList.Add('.cpt=image/x-corelphotopaint');
  aMimeList.Add('.cqk=application/x-calquick');
  aMimeList.Add('.crd=application/x-mscardfile');
  aMimeList.Add('.crl=application/pkix-crl');
  aMimeList.Add('.cs=text/x-csharp');
  aMimeList.Add('.csh=application/x-csh');
  aMimeList.Add('.css=text/css');
  aMimeList.Add('.csv=text/csv');
  aMimeList.Add('.cxx=text/x-c++src');
  aMimeList.Add('.dar=application/x-dar');
  aMimeList.Add('.dbf=application/x-dbase');
  aMimeList.Add('.dcr=application/x-director');
  aMimeList.Add('.deb=application/x-debian-package');
  aMimeList.Add('.dir=application/x-director');
  aMimeList.Add('.dist=vnd.apple.installer+xml');
  aMimeList.Add('.distz=vnd.apple.installer+xml');
  aMimeList.Add('.djv=image/vnd.djvu');
  aMimeList.Add('.djvu=image/vnd.djvu');
  aMimeList.Add('.dl=video/dl');
  aMimeList.Add('.dll=application/x-msdos-program');
  aMimeList.Add('.dmg=application/x-apple-diskimage');
  aMimeList.Add('.doc=application/vnd.ms-word');
  aMimeList.Add('.docx=application/vnd.openxmlformats-officedocument.wordprocessingml.document');
  aMimeList.Add('.dot=application/msword');
  aMimeList.Add('.dv=video/dv');
  aMimeList.Add('.dvi=application/x-dvi');
  aMimeList.Add('.dxr=application/x-director');
  aMimeList.Add('.ebk=application/x-expandedbook');
  aMimeList.Add('.eot=application/vnd.ms-fontobject');
  aMimeList.Add('.eps=application/postscript');
  aMimeList.Add('.epub=application/epub+zip');
  aMimeList.Add('.evy=application/envoy');
  aMimeList.Add('.exe=application/x-msdos-program');
  aMimeList.Add('.fdf=application/vnd.fdf');
  aMimeList.Add('.fif=application/fractals');
  aMimeList.Add('.flc=video/flc');
  aMimeList.Add('.fli=video/fli');
  aMimeList.Add('.flm=application/vnd.kde.kivio');
  aMimeList.Add('.fml=application/x-file-mirror-list');
  aMimeList.Add('.gif=image/gif');
  aMimeList.Add('.gl=video/gl');
  aMimeList.Add('.gnumeric=application/x-gnumeric');
  aMimeList.Add('.gsm=audio/x-gsm');
  aMimeList.Add('.gtar=application/x-gtar');
  aMimeList.Add('.gz=application/gzip');
  aMimeList.Add('.gzip=application/x-gzip');
  aMimeList.Add('.h=text/x-chdr');
  aMimeList.Add('.h++=text/x-c++hdr');
  aMimeList.Add('.hdf=application/x-hdf');
  aMimeList.Add('.hh=text/x-c++hdr');
  aMimeList.Add('.hlp=application/winhlp');
  aMimeList.Add('.hpf=application/x-icq-hpf');
  aMimeList.Add('.hpp=text/x-c++hdr');
  aMimeList.Add('.hqx=application/mac-binhex40');
  aMimeList.Add('.hta=application/hta');
  aMimeList.Add('.htc=text/x-component');
  aMimeList.Add('.htm=text/html');
  aMimeList.Add('.html=text/html');
  aMimeList.Add('.htt=text/webviewhtml');
  aMimeList.Add('.hxx=text/x-c++hdr');
  aMimeList.Add('.ico=image/vnd.microsoft.icon');
  aMimeList.Add('.ics=text/calendar');
  aMimeList.Add('.ief=image/ief');
  aMimeList.Add('.iii=application/x-iphone');
  aMimeList.Add('.ims=application/vnd.ms-ims');
  aMimeList.Add('.ins=application/x-internet-signup');
  aMimeList.Add('.iso=application/x-iso9660-image');
  aMimeList.Add('.ivf=video/x-ivf');
  aMimeList.Add('.jar=application/java-archive');
  aMimeList.Add('.java=text/x-java');
  aMimeList.Add('.jng=image/x-jng');
  aMimeList.Add('.jpe=image/jpeg');
  aMimeList.Add('.jpeg=image/jpeg');
  aMimeList.Add('.jpg=image/jpeg');
  aMimeList.Add('.js=text/javascript');
  aMimeList.Add('.json=application/json');
  aMimeList.Add('.jsonld=application/ld+json');
  aMimeList.Add('.kar=audio/midi');
  aMimeList.Add('.karbon=application/vnd.kde.karbon');
  aMimeList.Add('.kfo=application/vnd.kde.kformula');
  aMimeList.Add('.kon=application/vnd.kde.kontour');
  aMimeList.Add('.kpr=application/vnd.kde.kpresenter');
  aMimeList.Add('.kpt=application/vnd.kde.kpresenter');
  aMimeList.Add('.kwd=application/vnd.kde.kword');
  aMimeList.Add('.kwt=application/vnd.kde.kword');
  aMimeList.Add('.latex=application/x-latex');
  aMimeList.Add('.lcc=application/fastman');
  aMimeList.Add('.lha=application/x-lzh');
  aMimeList.Add('.lrm=application/vnd.ms-lrm');
  aMimeList.Add('.ls=text/javascript');
  aMimeList.Add('.lsf=video/x-la-asf');
  aMimeList.Add('.lsx=video/x-la-asf');
  aMimeList.Add('.lz=application/x-lzip');
  aMimeList.Add('.lzh=application/x-lzh');
  aMimeList.Add('.lzma=application/x-lzma');
  aMimeList.Add('.lzo=application/x-lzop');
  aMimeList.Add('.lzx=application/x-lzx');
  aMimeList.Add('.m13=application/x-msmediaview');
  aMimeList.Add('.m14=application/x-msmediaview');
  aMimeList.Add('.m3u=audio/mpegurl');
  aMimeList.Add('.m4a=audio/x-mpg');
  aMimeList.Add('.man=application/x-troff-man');
  aMimeList.Add('.mdb=application/x-msaccess');
  aMimeList.Add('.me=application/x-troff-me');
  aMimeList.Add('.mht=message/rfc822');
  aMimeList.Add('.mid=audio/midi');
  aMimeList.Add('.midi=audio/x-midi');
  aMimeList.Add('.mjf=audio/x-vnd.AudioExplosion.MjuiceMediaFile');
  aMimeList.Add('.mjs=text/javascript');
  aMimeList.Add('.mng=video/x-mng');
  aMimeList.Add('.mny=application/x-msmoney');
  aMimeList.Add('.mocha=text/javascript');
  aMimeList.Add('.moov=video/quicktime');
  aMimeList.Add('.mov=video/quicktime');
  aMimeList.Add('.movie=video/x-sgi-movie');
  aMimeList.Add('.mp2=audio/x-mpg');
  aMimeList.Add('.mp2=video/mpeg');
  aMimeList.Add('.mp3=audio/mpeg');
  aMimeList.Add('.mp3=video/mpeg');
  aMimeList.Add('.mp4=video/mp4');
  aMimeList.Add('.mp4=video/mpeg');
  aMimeList.Add('.mpa=video/mpeg');
  aMimeList.Add('.mpe=video/mpeg');
  aMimeList.Add('.mpeg=video/mpeg');
  aMimeList.Add('.mpega=audio/x-mpg');
  aMimeList.Add('.mpg=video/mpeg');
  aMimeList.Add('.mpga=audio/x-mpg');
  aMimeList.Add('.mpkg=application/vnd.apple.installer+xml');
  aMimeList.Add('.mpp=application/vnd.ms-project');
  aMimeList.Add('.ms=application/x-troff-ms');
  aMimeList.Add('.msi=application/x-msi');
  aMimeList.Add('.mvb=application/x-msmediaview');
  aMimeList.Add('.mxu=video/vnd.mpegurl');
  aMimeList.Add('.nix=application/x-mix-transfer');
  aMimeList.Add('.nml=animation/narrative');
  aMimeList.Add('.o=application/x-object');
  aMimeList.Add('.oda=application/oda');
  aMimeList.Add('.odb=application/vnd.oasis.opendocument.database');
  aMimeList.Add('.odc=application/vnd.oasis.opendocument.chart');
  aMimeList.Add('.odf=application/vnd.oasis.opendocument.formula');
  aMimeList.Add('.odg=application/vnd.oasis.opendocument.graphics');
  aMimeList.Add('.odi=application/vnd.oasis.opendocument.image');
  aMimeList.Add('.odm=application/vnd.oasis.opendocument.text-master');
  aMimeList.Add('.odp=application/vnd.oasis.opendocument.presentation');
  aMimeList.Add('.ods=application/vnd.oasis.opendocument.spreadsheet');
  aMimeList.Add('.odt=application/vnd.oasis.opendocument.text');
  aMimeList.Add('.oga=audio/ogg');
  aMimeList.Add('.ogg=application/ogg');
  aMimeList.Add('.ogv=video/ogg');
  aMimeList.Add('.ogx=application/ogg');
  aMimeList.Add('.opus=audio/opus');
  aMimeList.Add('.otf=font/otf');
  aMimeList.Add('.otg=application/vnd.oasis.opendocument.graphics-template');
  aMimeList.Add('.oth=application/vnd.oasis.opendocument.text-web');
  aMimeList.Add('.otp=application/vnd.oasis.opendocument.presentation-template');
  aMimeList.Add('.ots=application/vnd.oasis.opendocument.spreadsheet-template');
  aMimeList.Add('.ott=application/vnd.oasis.opendocument.text-template');
  aMimeList.Add('.p=text/x-pascal');
  aMimeList.Add('.p10=application/pkcs10');
  aMimeList.Add('.p12=application/x-pkcs12');
  aMimeList.Add('.p7b=application/x-pkcs7-certificates');
  aMimeList.Add('.p7m=application/pkcs7-mime');
  aMimeList.Add('.p7r=application/x-pkcs7-certreqresp');
  aMimeList.Add('.p7s=application/pkcs7-signature');
  aMimeList.Add('.package=application/vnd.autopackage');
  aMimeList.Add('.pas=text/x-pascal');
  aMimeList.Add('.pat=image/x-coreldrawpattern');
  aMimeList.Add('.pbm=image/x-portable-bitmap');
  aMimeList.Add('.pcx=image/pcx');
  aMimeList.Add('.pdf=application/pdf');
  aMimeList.Add('.pfr=application/font-tdpfr');
  aMimeList.Add('.pgm=image/x-portable-graymap');
  aMimeList.Add('.php=application/x-httpd-php');
  aMimeList.Add('.pict=image/x-pict');
  aMimeList.Add('.pkg=vnd.apple.installer+xml');
  aMimeList.Add('.pko=application/vnd.ms-pki.pko');
  aMimeList.Add('.pl=application/x-perl');
  aMimeList.Add('.pls=audio/x-scpls');
  aMimeList.Add('.png=image/png');
  aMimeList.Add('.pnm=image/x-portable-anymap');
  aMimeList.Add('.pnq=application/x-icq-pnq');
  aMimeList.Add('.pntg=image/x-macpaint');
  aMimeList.Add('.pot=application/mspowerpoint');
  aMimeList.Add('.ppm=image/x-portable-pixmap');
  aMimeList.Add('.pps=application/mspowerpoint');
  aMimeList.Add('.ppt=application/vnd.ms-powerpoint');
  aMimeList.Add('.pptx=application/vnd.openxmlformats-officedocument.presentationml.presentation');
  aMimeList.Add('.ppz=application/mspowerpoint');
  aMimeList.Add('.ps=application/postscript');
  aMimeList.Add('.psd=image/x-psd');
  aMimeList.Add('.pub=application/x-mspublisher');
  aMimeList.Add('.qcp=audio/vnd.qcelp');
  aMimeList.Add('.qpw=application/x-quattropro');
  aMimeList.Add('.qt=video/quicktime');
  aMimeList.Add('.qtc=video/x-qtc');
  aMimeList.Add('.qtif=image/x-quicktime');
  aMimeList.Add('.qtl=application/x-quicktimeplayer');
  aMimeList.Add('.ra=audio/x-realaudio');
  aMimeList.Add('.ram=audio/x-pn-realaudio');
  aMimeList.Add('.rar=application/vnd.rar');
  aMimeList.Add('.ras=image/x-cmu-raster');
  aMimeList.Add('.rdf=application/rdf+xml');
  aMimeList.Add('.rf=image/vnd.rn-realflash');
  aMimeList.Add('.rgb=image/x-rgb');
  aMimeList.Add('.rjs=application/vnd.rn-realsystem-rjs');
  aMimeList.Add('.rm=application/vnd.rn-realmedia');
  aMimeList.Add('.rmf=application/vnd.rmf');
  aMimeList.Add('.rmp=application/vnd.rn-rn_music_package');
  aMimeList.Add('.rms=video/vnd.rn-realvideo-secure');
  aMimeList.Add('.rmx=application/vnd.rn-realsystem-rmx');
  aMimeList.Add('.rnx=application/vnd.rn-realplayer');
  aMimeList.Add('.rp=image/vnd.rn-realpix');
  aMimeList.Add('.rpm=application/x-redhat-package-manager');
  aMimeList.Add('.rsml=application/vnd.rn-rsml');
  aMimeList.Add('.rss=application/rss+xml');
  aMimeList.Add('.rt=text/vnd.rn-realtext');
  aMimeList.Add('.rtf=application/rtf');
  aMimeList.Add('.rtsp=application/x-rtsp');
  aMimeList.Add('.rtx=text/richtext');
  aMimeList.Add('.rv=video/vnd.rn-realvideo');
  aMimeList.Add('.scd=application/x-msschedule');
  aMimeList.Add('.scm=application/x-icq-scm');
  aMimeList.Add('.sd2=audio/x-sd2');
  aMimeList.Add('.sda=application/vnd.stardivision.draw');
  aMimeList.Add('.sdc=application/vnd.stardivision.calc');
  aMimeList.Add('.sdd=application/vnd.stardivision.impress');
  aMimeList.Add('.sdp=application/x-sdp');
  aMimeList.Add('.ser=application/java-serialized-object');
  aMimeList.Add('.setpay=application/set-payment-initiation');
  aMimeList.Add('.setreg=application/set-registration-initiation');
  aMimeList.Add('.sgi=image/x-sgi');
  aMimeList.Add('.sgm=text/sgml');
  aMimeList.Add('.sgml=text/sgml');
  aMimeList.Add('.sh=application/x-sh');
  aMimeList.Add('.shar=application/x-shar');
  aMimeList.Add('.shtml=server-parsed-html');
  aMimeList.Add('.shw=application/presentations');
  aMimeList.Add('.sid=audio/prs.sid');
  aMimeList.Add('.sit=application/x-stuffit');
  aMimeList.Add('.sitx=application/x-stuffitx');
  aMimeList.Add('.skd=application/x-koan');
  aMimeList.Add('.skm=application/x-koan');
  aMimeList.Add('.skp=application/x-koan');
  aMimeList.Add('.skt=application/x-koan');
  aMimeList.Add('.smf=application/vnd.stardivision.math');
  aMimeList.Add('.smi=application/smil');
  aMimeList.Add('.smil=application/smil');
  aMimeList.Add('.snd=audio/basic');
  aMimeList.Add('.spl=application/futuresplash');
  aMimeList.Add('.ssm=application/streamingmedia');
  aMimeList.Add('.sst=application/vnd.ms-pki.certstore');
  aMimeList.Add('.stc=application/vnd.sun.xml.calc.template');
  aMimeList.Add('.stdptoms=application/vnd.sun.xml.draw.template');
  aMimeList.Add('.sti=application/vnd.sun.xml.impress.template');
  aMimeList.Add('.stl=application/vnd.ms-pki.stl');
  aMimeList.Add('.stw=application/vnd.sun.xml.writer.template');
  aMimeList.Add('.sv4cpio=application/x-sv4cpio');
  aMimeList.Add('.sv4crc=application/x-sv4crc');
  aMimeList.Add('.svg=image/svg+xml');
  aMimeList.Add('.svgz=image/svg+xml');
  aMimeList.Add('.svi=application/softvision');
  aMimeList.Add('.swf=application/x-shockwave-flash');
  aMimeList.Add('.swf1=application/x-shockwave-flash');
  aMimeList.Add('.sxc=application/vnd.sun.xml.calc');
  aMimeList.Add('.sxg=application/vnd.sun.xml.writer.global');
  aMimeList.Add('.sxi=application/vnd.sun.xml.impress');
  aMimeList.Add('.sxm=application/vnd.sun.xml.math');
  aMimeList.Add('.sxw=application/vnd.sun.xml.writer');
  aMimeList.Add('.t=application/x-troff');
  aMimeList.Add('.tar=application/x-tar');
  aMimeList.Add('.targa=image/x-targa');
  aMimeList.Add('.tbz=application/x-bzip-compressed-tar');
  aMimeList.Add('.tbz2=application/x-bzip-compressed-tar');
  aMimeList.Add('.tcl=application/x-tcl');
  aMimeList.Add('.tex=application/x-tex');
  aMimeList.Add('.texi=application/x-texinfo');
  aMimeList.Add('.texinfo=application/x-texinfo');
  aMimeList.Add('.tgz=application/x-compressed-tar');
  aMimeList.Add('.tif=image/tiff');
  aMimeList.Add('.tiff=image/tiff');
  aMimeList.Add('.tlz=application/x-lzma-compressed-tar');
  aMimeList.Add('.torrent=application/x-bittorrent');
  aMimeList.Add('.tr=application/x-troff');
  aMimeList.Add('.trm=application/x-msterminal');
  aMimeList.Add('.troff=application/x-troff');
  aMimeList.Add('.ts=video/mp2t');
  aMimeList.Add('.tsp=application/dsptype');
  aMimeList.Add('.ttf=font/ttf');
  aMimeList.Add('.ttz=application/t-time');
  aMimeList.Add('.txt=text/plain');
  aMimeList.Add('.txz=application/x-xz-compressed-tar');
  aMimeList.Add('.udeb=application/x-debian-package');
  aMimeList.Add('.uin=application/x-icq');
  aMimeList.Add('.uls=text/iuls');
  aMimeList.Add('.urls=application/x-url-list');
  aMimeList.Add('.ustar=application/x-ustar');
  aMimeList.Add('.vcd=application/x-cdlink');
  aMimeList.Add('.vcf=text/x-vcard');
  aMimeList.Add('.vor=application/vnd.stardivision.writer');
  aMimeList.Add('.vsd=application/vnd.visio');
  aMimeList.Add('.vsl=application/x-cnet-vsl');
  aMimeList.Add('.wav=audio/wav');
  aMimeList.Add('.wax=audio/x-ms-wax');
  aMimeList.Add('.wb1=application/x-quattropro');
  aMimeList.Add('.wb2=application/x-quattropro');
  aMimeList.Add('.wb3=application/x-quattropro');
  aMimeList.Add('.wbmp=image/vnd.wap.wbmp');
  aMimeList.Add('.wcm=application/vnd.ms-works');
  aMimeList.Add('.wdb=application/vnd.ms-works');
  aMimeList.Add('.weba=audio/webm');
  aMimeList.Add('.webm=video/webm');
  aMimeList.Add('.webp=image/webp');
  aMimeList.Add('.wks=application/vnd.ms-works');
  aMimeList.Add('.wm=video/x-ms-wm');
  aMimeList.Add('.wma=audio/x-ms-wma');
  aMimeList.Add('.wmd=application/x-ms-wmd');
  aMimeList.Add('.wml=text/vnd.wap.wml');
  aMimeList.Add('.wmlc=application/vnd.wap.wmlc');
  aMimeList.Add('.wmls=text/vnd.wap.wmlscript');
  aMimeList.Add('.wmlsc=application/vnd.wap.wmlscriptc');
  aMimeList.Add('.wmp=video/x-ms-wmp');
  aMimeList.Add('.wms=application/x-ms-wms');
  aMimeList.Add('.wmv=video/x-ms-wmv');
  aMimeList.Add('.wmx=video/x-ms-wmx');
  aMimeList.Add('.wmz=application/x-ms-wmz');
  aMimeList.Add('.woff=font/woff');
  aMimeList.Add('.woff2=font/woff2');
  aMimeList.Add('.wp5=application/wordperfect5.1');
  aMimeList.Add('.wpd=application/wordperfect');
  aMimeList.Add('.wpl=application/vnd.ms-wpl');
  aMimeList.Add('.wps=application/vnd.ms-works');
  aMimeList.Add('.wri=application/x-mswrite');
  aMimeList.Add('.wsc=text/scriptlet');
  aMimeList.Add('.wvx=video/x-ms-wvx');
  aMimeList.Add('.xbm=image/x-xbitmap');
  aMimeList.Add('.xfdf=application/vnd.adobe.xfdf');
  aMimeList.Add('.xht=application/xhtml+xml');
  aMimeList.Add('.xhtml=application/xhtml+xml');
  aMimeList.Add('.xlb=application/x-msexcel');
  aMimeList.Add('.xls=application/vnd.ms-excel');
  aMimeList.Add('.xlsx=application/vnd.openxmlformats-officedocument.spreadsheetml.sheet');
  aMimeList.Add('.xml=application/xml');
  aMimeList.Add('.xpi=application/x-xpinstall');
  aMimeList.Add('.xpm=image/x-xpixmap');
  aMimeList.Add('.xps=application/vnd.ms-xpsdocument');
  aMimeList.Add('.xsd=application/vnd.sun.xml.draw');
  aMimeList.Add('.xul=application/vnd.mozilla.xul+xml');
  aMimeList.Add('.xwd=image/x-xwindowdump');
  aMimeList.Add('.z=application/x-compress');
  aMimeList.Add('.zip=application/zip');
  aMimeList.Add('.zoo=application/x-zoo');
  aMimeList.Add('.apk=application/vnd.android.package-archive');
  aMimeList.EndUpdate;
end;

function TBadgerUtils.GetFileMIMEType(const AFileName: string): string;
var
  Index: Integer;
  LExt: string;
begin
  BuildMimelist(FMimeList);
  LExt := LowerCase(ExtractFileExt(AFileName));
  Index := FMimeList.IndexOfName(LExt);
  if (Index = -1) and (FMimeList.Count = 0) then
  begin
    Index := FMimeList.IndexOf(LExt);
  end;
  if Index <> -1 then
    Result := FMimeList.ValueFromIndex[Index]
  else
    Result := 'application/octet-stream';
end;

function CustomEncodeBase64(const Input: string; URLSafe: Boolean): string;
var
  Bytes: TBytes;
  i, Len, Pos: Integer;
  OutLen: Integer;
  Buffer: array [0 .. 3] of Char;
  RemainingBytes: Integer;
  TempIndex: Integer;
begin
  SetLength(Bytes, Length(Input));
  for i := 1 to Length(Input) do
    Bytes[i - 1] := Byte(AnsiChar(Input[i]));

  Len := Length(Bytes);
  OutLen := ((Len + 2) div 3) * 4;
  SetLength(Result, OutLen);
  Pos := 1;
  i := 0;

  while i < Len do
  begin
    RemainingBytes := Len - i;

    Buffer[0] := Base64Alphabet[(Bytes[i] shr 2) and 63];

    if RemainingBytes > 1 then
      Buffer[1] := Base64Alphabet[((Bytes[i] shl 4) or ((Bytes[i + 1] shr 4) and 15)) and 63]
    else
      Buffer[1] := Base64Alphabet[(Bytes[i] shl 4) and 63];

    if RemainingBytes > 1 then
    begin
      TempIndex := (Bytes[i + 1] shl 2) and 63;
      if RemainingBytes > 2 then
        TempIndex := TempIndex or ((Bytes[i + 2] shr 6) and 3);
      Buffer[2] := Base64Alphabet[TempIndex and 63];
    end
    else
      Buffer[2] := '=';

    if RemainingBytes > 2 then
      Buffer[3] := Base64Alphabet[Bytes[i + 2] and 63]
    else
      Buffer[3] := '=';

    Result[Pos] := Buffer[0];
    Result[Pos + 1] := Buffer[1];
    Result[Pos + 2] := Buffer[2];
    Result[Pos + 3] := Buffer[3];

    Inc(i, 3);
    Inc(Pos, 4);
  end;

  if URLSafe then
  begin
    Result := StringReplace(Result, '+', '-', [rfReplaceAll]);
    Result := StringReplace(Result, '/', '_', [rfReplaceAll]);
    Result := StringReplace(Result, '=', '', [rfReplaceAll]);
  end;
end;

function CustomDecodeBase64(const Input: string): string;
var
  Bytes: TBytes;
  i, Len, Pos: Integer;
  InBuf: array [0 .. 3] of Byte;
  OutBuf: array [0 .. 2] of Byte;
  Base64Table: array [Char] of Byte;
  CleanInput: string;
begin
  FillChar(Base64Table, SizeOf(Base64Table), 255);
  for i := 0 to 63 do
    Base64Table[Base64Alphabet[i]] := i;
  Base64Table['-'] := Base64Table['+'];
  Base64Table['_'] := Base64Table['/'];

  CleanInput := StringReplace(Input, '-', '+', [rfReplaceAll]);
  CleanInput := StringReplace(CleanInput, '_', '/', [rfReplaceAll]);
  case Length(CleanInput) mod 4 of
    2:
      CleanInput := CleanInput + '==';
    3:
      CleanInput := CleanInput + '=';
  end;

  Len := Length(CleanInput);
  SetLength(Bytes, (Len * 3) div 4);
  Pos := 0;

  i := 1;
  while i <= Len do
  begin
    InBuf[0] := Base64Table[CleanInput[i]];
    InBuf[1] := Base64Table[CleanInput[i + 1]];
    InBuf[2] := Base64Table[CleanInput[i + 2]];
    InBuf[3] := Base64Table[CleanInput[i + 3]];

    OutBuf[0] := (InBuf[0] shl 2) or ((InBuf[1] shr 4) and 3);
    OutBuf[1] := ((InBuf[1] shl 4) and $F0) or ((InBuf[2] shr 2) and $0F);
    OutBuf[2] := ((InBuf[2] shl 6) and $C0) or (InBuf[3] and $3F);

    Bytes[Pos] := OutBuf[0];
    if CleanInput[i + 2] <> '=' then
      Bytes[Pos + 1] := OutBuf[1];
    if CleanInput[i + 3] <> '=' then
      Bytes[Pos + 2] := OutBuf[2];

    Inc(i, 4);
    Inc(Pos, 3);
  end;

  if CleanInput[Len] = '=' then
    Dec(Pos);
  if CleanInput[Len - 1] = '=' then
    Dec(Pos);
  SetLength(Bytes, Pos);

  SetLength(Result, Length(Bytes));
  for i := 0 to Length(Bytes) - 1 do
    Result[i + 1] := Chr(Bytes[i]);
end;

function BytesToRawString(const ABytes: TBytes): string;
var
  i: Integer;
begin
  SetLength(Result, Length(ABytes));
  for i := 0 to Length(ABytes) - 1 do
    Result[i + 1] := Chr(ABytes[i]);
end;

function RawStringToBytes(const S: string): TBytes;
var
  i: Integer;
begin
  SetLength(Result, Length(S));
  for i := 1 to Length(S) do
    Result[i - 1] := Byte(AnsiChar(S[i]));
end;

end.
