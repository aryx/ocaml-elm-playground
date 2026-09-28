(* Asn1: reading DER, the bytes certificates are written in (ITU-T
   X.690, 1988; ASN.1, the notation, X.680).

   Every value is TLV -- a tag byte, a length, the value's bytes:

       30 82 01 0a  ...        SEQUENCE (30), 266 bytes long (82: the
                               length in the next 2 bytes, 01 0a)
          02 01 05             INTEGER, 1 byte: 5
          06 03 55 1d 11       OBJECT IDENTIFIER 2.5.29.17 (subjectAltName)
          a3 ...               [3], context-specific, constructed:
                               what the schema says tag 3 means here

   A constructed value's bytes are more TLVs (a SEQUENCE's fields); a
   primitive's are the thing itself. DER is BER's one Distinguished way
   to write each value -- the shortest length, no indefinite ones -- so
   that a signature over the bytes is a signature over the value.

   Worked examples (checked by the tests): 30 06 02 01 05 01 01 ff is a
   SEQUENCE of the INTEGER 5 and the BOOLEAN true; 06 08 2a 86 48 86 f7
   0d 01 01 is the OID 1.2.840.113549.1.1 (base 128, the first two
   numbers packed as 40*1+2); a certificate's whole tree (X509's tests).

   References: ITU-T X.690 (2021), sections 8 and 10; Burton Kaliski,
   "A Layman's Guide to a Subset of ASN.1, BER, and DER" (RSA
   Laboratories, 1993). *)

type t = {
  tag : int; (* the identifier byte: 0x30 SEQUENCE, 0x02 INTEGER, 0xa3 [3]... *)
  value : string; (* the content bytes *)
  raw : string; (* the whole TLV, as it was: what a signature covers *)
}

(* the value at the start of the bytes, and where it ended *)
val parse : string -> int -> (t * int, string) result

(* a constructed value's elements; [] for a primitive one *)
val children : t -> t list

(* the value of an OBJECT IDENTIFIER, dotted: "1.2.840.113549.1.1.11" *)
val oid : t -> string

(* an INTEGER as a natural (a negative one is an error in certificates) *)
val integer : t -> Bignum.t

(* an OCTET STRING's bytes (the value itself) *)
val value_of : t -> string

(* a BIT STRING's bytes, less the unused-bits count *)
val bit_string : t -> string

(* UTCTime (YYMMDDHHMMSSZ) or GeneralizedTime (YYYYMMDDHHMMSSZ), as
 * seconds since the epoch *)
val time : t -> float option

(* the text of a string type (UTF8String, PrintableString, IA5String...) *)
val text : t -> string
