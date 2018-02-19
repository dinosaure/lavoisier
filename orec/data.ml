type mut = Mut
type imm = Imm

type ('t, 'c) getter = Get
type +'k updater = Updater

type top  = Top
type only = Bottom

type 'a fn    = top * 'a * 'a
type 'a const = 'a * top * 'a
type 'a del   = 'a * 'a * top

type any = top * top * top

type ('ty, 'fy, 'm) storage =
  | Imm : ('a, 'a, imm) storage
  | Mut : ('a, 'a ref, mut) storage

type ('f, 't) access =
  | Opt : ('a, 'a option) access
  | Exn : ('a, 'a) access
