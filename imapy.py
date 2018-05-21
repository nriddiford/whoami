#!/usr/bin/env python
#
# Very basic example of using Python and IMAP to iterate over emails in a
# gmail folder/label.  This code is released into the public domain.
#
# RKI July 2013
# http://www.voidynullness.net/blog/2013/07/25/gmail-email-with-python-via-imap/
#
import sys
import imaplib
import getpass
import email
import email.header
import datetime

from email_reply_parser import EmailReplyParser

EMAIL_ACCOUNT = "nickriddiford@gmail.com"
EMAIL_FOLDER = "[Gmail]/Sent Mail"


def process_mailbox(M):
    """
    Do something with emails messages in the folder.
    For the sake of this example, print some headers.
    """

    rv, data = M.search(None, "ALL")
    if rv != 'OK':
        print "No messages found!"
        return

    with open('emails2.tsv', 'w') as outFile:
        for num in data[0].split():
            rv, data = M.fetch(num, '(RFC822)')
            if rv != 'OK':
                print "ERROR getting message", num
                return

            msg = email.message_from_string(data[0][1])
            decode = email.header.decode_header(msg['Subject'])[0]

            body = ""

            if msg.is_multipart():
                for part in msg.walk():
                    ctype = part.get_content_type()
                    cdispo = str(part.get('Content-Disposition'))

                    # skip any text/plain (txt) attachments
                    if ctype == 'text/plain' and 'attachment' not in cdispo:
                        body = part.get_payload(decode=True)  # decode
                        break
            # not multipart - i.e. plain text, no attachments, keeping fingers crossed
            else:
                body = msg.get_payload(decode=True)


            # print 'Message %s: %s' % (num, subject)
            # print 'Raw Date:', msg['Date']
            # print 'Adressee:', msg['to']
            # print body

            # Now convert to local date-time
            date_tuple = email.utils.parsedate_tz(msg['Date'])
            if date_tuple:
                local_date = datetime.datetime.fromtimestamp(
                    email.utils.mktime_tz(date_tuple))
                send_date = local_date.strftime("%a, %d %b %Y %H:%M:%S")

            reply = EmailReplyParser.parse_reply(body)

            reply_string = reply.replace('\n', ' ').replace('\r', '')
            reply_string = email.utils.decode_rfc2231(reply_string)

            outFile.write("%s\t%s\t%s\t%s\n" % (msg['from'], msg['to'], send_date, reply_string))


M = imaplib.IMAP4_SSL('imap.gmail.com')

try:
    rv, data = M.login(EMAIL_ACCOUNT, getpass.getpass())
except imaplib.IMAP4.error:
    print "LOGIN FAILED!!! "
    sys.exit(1)

print rv, data

rv, mailboxes = M.list()
if rv == 'OK':
    print "Mailboxes:"
    print mailboxes

rv, data = M.select(EMAIL_FOLDER)
if rv == 'OK':
    print "Processing mailbox...\n"
    process_mailbox(M)
    M.close()
else:
    print "ERROR: Unable to open mailbox ", rv

M.logout()