# hashflash

Email autoresponder service for geohash coordinates.

## Usage

Clone the project and use Leiningen to launch the service:

```bash
lein run -m org.timmc.hashflash path/to/config.clj
```

A config file looks like this:

```clojure
{:imap-domain "imap.gmail.com"
 :smtp-domain "smtp.gmail.com"
 :smtp-port 465
 :username "srv@geohash.info"
 :password "locative2"
 :source-folder "Inbox"
 :reject-folder "rejected"
 :done-folder "answered"}
```

Because IMAP implementations vary quite a bit (\*cough\* Google \*cough\*),
I can't guarantee this works on any service other than GMail.

## License

Copyright © 2013 Tim McCormack

Distributed under the Eclipse Public License, the same as Clojure.
