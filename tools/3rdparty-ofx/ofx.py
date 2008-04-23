#!/usr/bin/python
import time, os, httplib, urllib2
import sys

join = str.join

sites = {
       "ucard": {
                 "caps": [ "SIGNON", "CCSTMT" ],
                  "fid": "24909",
                "fiorg": "Citigroup",
                  "url": "https://secureofx2.bankhost.com/citi/cgi-forte/ofx_rt?servicename=ofx_rt&pagename=ofx",
                },
    "discover": {
                 "caps": [ "SIGNON", "CCSTMT" ],
                "fiorg": "Discover Financial Services",
                  "fid": "7101",
                  "url": "https://ofx.discovercard.com/",
                },
     "ameritrade": {
                 "caps": [ "SIGNON", "INVSTMT" ],
                "fiorg": "ameritrade.com",
                  "url": "https://ofx.ameritrade.com/ofxproxy/ofx_proxy.dll",
                } 
    }

def _field(tag,value):
    return "<"+tag+">"+value

def _tag(tag,*contents):
    return join("\r\n",["<"+tag+">"]+list(contents)+["</"+tag+">"])

def _date():
    return time.strftime("%Y%m%d%H%M%S",time.localtime())

def _genuuid():
    return os.popen("uuidgen").read().rstrip().upper()

class OFXClient:
    """Encapsulate an ofx client, config is a dict containg configuration"""
    def __init__(self, config, user, password):
        self.password = password
        self.user = user
        self.config = config
        self.cookie = 3
        config["user"] = user
        config["password"] = password
        if not config.has_key("appid"):
            config["appid"] = "PyOFX"
            config["appver"] = "0100"

    def _cookie(self):
        self.cookie += 1
        return str(self.cookie)

    """Generate signon message"""
    def _signOn(self):
        config = self.config
        fidata = [ _field("ORG",config["fiorg"]) ]
        if config.has_key("fid"):
            fidata += [ _field("FID",config["fid"]) ]
        return _tag("SIGNONMSGSRQV1",
                    _tag("SONRQ",
                         _field("DTCLIENT",_date()),
                         _field("USERID",config["user"]),
                         _field("USERPASS",config["password"]),
                         _field("LANGUAGE","ENG"),
                         _tag("FI", *fidata),
                         _field("APPID",config["appid"]),
                         _field("APPVER",config["appver"]),
                         ))

    def _acctreq(self, dtstart):
        req = _tag("ACCTINFORQ",_field("DTACCTUP",dtstart))
        return self._message("SIGNUP","ACCTINFO",req)

    def _ccreq(self, acctid, dtstart):
        config=self.config
        req = _tag("CCSTMTRQ",
                   _tag("CCACCTFROM",_field("ACCTID",acctid)),
                   _tag("INCTRAN",
                        _field("DTSTART",dtstart),
                        _field("INCLUDE","Y")))
        return self._message("CREDITCARD","CCSTMT",req)

    def _invstreq(self, brokerid, acctid, dtstart):
        dtnow = time.strftime("%Y%m%d%H%M%S",time.localtime())
        req = _tag("INVSTMTRQ",
                   _tag("INVACCTFROM",
                      _field("BROKERID", brokerid),
                      _field("ACCTID",acctid)),
                   _tag("INCTRAN",
                        _field("DTSTART",dtstart),
                        _field("INCLUDE","Y")),
                   _field("INCOO","Y"),
                   _tag("INCPOS",
                        _field("DTASOF", dtnow),
                        _field("INCLUDE","Y")),
                   _field("INCBAL","Y"))
        return self._message("INVSTMT","INVSTMT",req)

    def _message(self,msgType,trnType,request):
        config = self.config
        return _tag(msgType+"MSGSRQV1",
                    _tag(trnType+"TRNRQ",
                         _field("TRNUID",_genuuid()),
                         _field("CLTCOOKIE",self._cookie()),
                         request))
    
    def _header(self):
        return join("\r\n",[ "OFXHEADER:100",
                           "DATA:OFXSGML",
                           "VERSION:102",
                           "SECURITY:NONE",
                           "ENCODING:USASCII",
                           "CHARSET:1252",
                           "COMPRESSION:NONE",
                           "OLDFILEUID:NONE",
                           "NEWFILEUID:"+_genuuid(),
                           ""])

    def ccQuery(self, acctid, dtstart):
        """CC Statement request"""
        return join("\r\n",[self._header(),
                          _tag("OFX",
                               self._signOn(),
                               self._ccreq(acctid, dtstart))])

    def acctQuery(self,dtstart):
        return join("\r\n",[self._header(),
                          _tag("OFX",
                               self._signOn(),
                               self._acctreq(dtstart))])

    def invstQuery(self, brokerid, acctid, dtstart):
        return join("\r\n",[self._header(),
                          _tag("OFX",
                               self._signOn(),
                               self._invstreq(brokerid, acctid,dtstart))])

    def doQuery(self,query,name):
        # N.B. urllib doesn't honor user Content-type, use urllib2
        request = urllib2.Request(self.config["url"],
                                  query,
                                  { "Content-type": "application/x-ofx",
                                    "Accept": "*/*, application/x-ofx"
                                  })
        if 1:
            f = urllib2.urlopen(request)
            response = f.read()
            f.close()
            
            f = file(name,"w")
            f.write(response)
            f.close()
	else:
            print request
            print self.config["url"], query
        
        # ...

import getpass
argv = sys.argv
if __name__=="__main__":
    dtstart = time.strftime("%Y%m%d",time.localtime(time.time()-31*86400))
    dtnow = time.strftime("%Y%m%d",time.localtime())
    if len(argv) < 3:
        print "Usage:",sys.argv[0], "site user [account]"
        print "available sites:",join(", ",sites.keys())
        sys.exit()
    passwd = getpass.getpass()
    client = OFXClient(sites[argv[1]], argv[2], passwd)
    if len(argv) < 4:
       query = client.acctQuery("19700101000000")
       client.doQuery(query, argv[1]+"_acct.ofx") 
    else:
       if "CCSTMT" in sites[argv[1]]["caps"]:
          query = client.ccQuery(sys.argv[3], dtstart)
       elif "INVSTMT" in sites[argv[1]]["caps"]:
          query = client.invstQuery(sites[argv[1]]["fiorg"], sys.argv[3], dtstart)
       client.doQuery(query, argv[1]+dtnow+".ofx")

