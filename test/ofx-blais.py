#!/usr/bin/python
import time, os, httplib, urllib2
import sys

join = str.join

sites = {
    "hsbc": {
        "caps": [ "SIGNON", "BASTMT" ],
        "fid": "9901",
        "fiorg": "ISC", 
        "url": "https://pfm.ebank.us.hsbc.com/prod.ofx",
        "bankid": "122240861", # bank routing #  // swift would be: MRMDUS33
        }	
    }
												
def _field(tag,value):
    return "<%s>%s</%s>" % (tag, value, tag)

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
            config["appid"] = "QWIN"  # i've had to fake Quicken to actually get my unwilling test server to talk to me
            config["appver"] = "1200"

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

# this is from _ccreq below and reading page 176 of the latest OFX doc.
    def _bareq(self, acctid, dtstart, accttype):
    	config=self.config
	req = _tag("STMTRQ",
		   _tag("BANKACCTFROM",
		   	_field("BANKID",sites [argv[1]] ["bankid"]),
		        _field("ACCTID",acctid),
			_field("ACCTTYPE",accttype)),
		   _tag("INCTRAN",
		   	_field("DTSTART",dtstart),
			_field("INCLUDE","Y")))
	return self._message("BANK","STMT",req)
	
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

    def baQuery(self, acctid, dtstart, accttype):
    	"""Bank account statement request"""
        return join("\r\n",[self._header(),
 	                  _tag("OFX",
                                self._signOn(),
                                self._bareq(acctid, dtstart, accttype))])
						
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
        print self.config['url']
        print query
        request = urllib2.Request(self.config["url"],
                                  query,
                                  { "Content-type": "application/x-ofx",
                                    "Accept": "*/*, application/x-ofx"
                                  })
        if 1:
            f = urllib2.urlopen(request)
            response = f.read()
            f.close()
            print response
            
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
        print "Usage:",sys.argv[0], "site user [account] [CHECKING/SAVINGS/.. if using BASTMT]"
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
       elif "BASTMT" in sites[argv[1]]["caps"]:
          query = client.baQuery(sys.argv[3], dtstart, sys.argv[4])
       client.doQuery(query, argv[1]+dtnow+".ofx")

