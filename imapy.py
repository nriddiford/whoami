#!/usr/bin/env python
#
# Heavily inspired by:
# http://www.voidynullness.net/blog/2013/07/25/gmail-email-with-python-via-imap/
#
import sys
import imaplib
import getpass
import email
import email.header
import datetime

import time
start_time = time.time()

from optparse import OptionParser

from email_reply_parser import EmailReplyParser

EMAIL_FOLDER = "[Gmail]/Sent Mail"


def process_mailbox(M, options):
    """
    Do something with emails messages in the folder.
    For the sake of this example, print some headers.
    """

    rv, data = M.search(None, "ALL")
    if rv != 'OK':
        print("No messages found!")
        return

    with open(options.output_file, 'w') as outFile:
        message_count = 0
        for num in data[0].split():
            rv, data = M.fetch(num, '(RFC822)')
            if rv != 'OK':
                print("ERROR getting message", num)
                return
            if message_count % 100 == 0 and message_count > 0:
                runTime = round((time.time() - start_time),1)
                print("Read %d messages in %s seconds") % (message_count, runTime)

            message_count += 1

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

           # Now convert to local date-time
            date_tuple = email.utils.parsedate_tz(msg['Date'])
            if date_tuple:
                local_date = datetime.datetime.fromtimestamp(
                    email.utils.mktime_tz(date_tuple))
                send_date = local_date.strftime("%a, %d %b %Y %H:%M:%S")

            reply = EmailReplyParser.parse_reply(body)

            # reply_string = [str(r) for r in reply]
            reply_string = reply.replace('\n', ' ').replace('\r', '')
            # reply_string = email.utils.decode_rfc2231(reply_string)

            outFile.write("%s\t%s\t%s\t%s\n" % (msg['from'], msg['to'], send_date, reply_string))


def get_args():
    parser = OptionParser()

    parser.add_option("-e",
                      "--email_address",
                      dest="email_address",
                      action="store",
                      help="Your Gmail email address")

    parser.add_option("-o",
                      "--output_file",
                      dest="output_file",
                      action="store",
                      help="Name of output file [Default: 'emails.tsv'")

    parser.set_defaults(output_file='emails.tsv')

    options, args = parser.parse_args()
    return options, args



def main():
    options, args = get_args()
    print(options)

    M = imaplib.IMAP4_SSL('imap.gmail.com')

    if options.email_address:
        try:
            rv, data = M.login(options.email_address, getpass.getpass())
        except imaplib.IMAP4.error:
            print("LOGIN FAILED!!! ")
            sys.exit(1)

        print(rv, data)

        rv, mailboxes = M.list()
        # if rv == 'OK':
        #     print "Mailboxes:"
        #     print mailboxes

        rv, data = M.select(EMAIL_FOLDER)
        if rv == 'OK':
            print("Processing mailbox...\n")
            process_mailbox(M, options)
            M.close()
        else:
            print("ERROR: Unable to open mailbox ", rv)

        M.logout()


if __name__ == "__main__":
    sys.exit(main())
