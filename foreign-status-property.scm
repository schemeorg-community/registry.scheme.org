(id set)
(description "Which set of status codes")
(type "symbol")

(id code)
(description "Numeric or pseudo-numeric status code")
(type "exact integer or symbol")

(id symbol)
(description "Mnemonic status identifier")
(type "symbol")

(id message)
(description "Brief human-readable status message")
(type "string")

(id message-locales)
(description "Languages in which message is available")
(type "list of BCP 47 strings")

(id message-detail)
(description "Detailed human-readable status message")
(type "string")

(id message-hint)
(description "Human-readable troubleshooting tips")
(type "string")

(id filename)
(description "File that this status concerns")
(type "string")

(id line-number)
(description "Line number in text")
(type "positive exact integer")

(id column-number)
(description "Column number on this line of text")
(type "positive exact integer")

(id sqlstate)
(description "5-character ANSI/ODBC SQLSTATE code")
(type "string")

(id class-code)
(description "Numeric or pseudo-numeric code of the status class")
(type "exact integer or symbol")

(id class-title)
(description "Human-readable name of the status class")
(type "string")

(id facility)
(description "syslog-style facility")
(type "symbol")

(id severity)
(description "syslog-style how severe the situation is")
(type "symbol")
