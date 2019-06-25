(* $Id: def.mli,v 5.22 2008-01-08 11:58:46 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

type ('a, 'b) choice =
    Left of 'a
  | Right of 'b

type iper = Adef.iper
type ifam = Adef.ifam

type cdate = Adef.cdate

type date =
  Adef.date =
      Dgreg of dmy * calendar
    | Dtext of string
and calendar = Adef.calendar = Dgregorian | Djulian | Dfrench | Dhebrew
and dmy =
  Adef.dmy =
    { day : int; month : int; year : int; prec : precision; delta : int }
and dmy2 = Adef.dmy2 = { day2 : int; month2 : int; year2 : int; delta2 : int }
and precision =
  Adef.precision =
      Sure
    | About
    | Maybe
    | Before
    | After
    | OrYear of dmy2
    | YearInt of dmy2

type f_relation_kind =
  | Married
  | NotMarried
  | Engaged
  | NoSexesCheckNotMarried
  | NoMention
  | NoSexesCheckMarried
  | MarriageBann
  | MarriageContract
  | MarriageLicense
  | Pacs
  | Residence

type relation_kind = f_relation_kind

type divorce =
  | NotDivorced
  | Divorced of cdate
  | Separated

type death_reason = Killed | Murdered | Executed | Disappeared | Unspecified
type death =
    NotDead
  | Death of death_reason * cdate
  | DeadYoung
  | DeadDontKnowWhen
  | DontKnowIfDead
  | OfCourseDead

type burial =
    UnknownBurial
  | Buried of cdate
  | Cremated of cdate

type access = IfTitles | Public | Private

type 'string gen_title_name =
    Tmain
  | Tname of 'string
  | Tnone
type 'string gen_title =
  { t_name : 'string gen_title_name;
    t_ident : 'string;
    t_place : 'string;
    t_date_start : cdate;
    t_date_end : cdate;
    t_nth : int }

type witness_kind = Witness | Witness_GodParent

type 'string gen_pers_event_name =
    Epers_Birth
  | Epers_Baptism
  | Epers_Death
  | Epers_Burial
  | Epers_Cremation
  | Epers_Accomplishment
  | Epers_Acquisition
  | Epers_Adhesion
  | Epers_BaptismLDS
  | Epers_BarMitzvah
  | Epers_BatMitzvah
  | Epers_Benediction
  | Epers_ChangeName
  | Epers_Circumcision
  | Epers_Confirmation
  | Epers_ConfirmationLDS
  | Epers_Decoration
  | Epers_DemobilisationMilitaire
  | Epers_Diploma
  | Epers_Distinction
  | Epers_Dotation
  | Epers_DotationLDS
  | Epers_Education
  | Epers_Election
  | Epers_Emigration
  | Epers_Excommunication
  | Epers_FamilyLinkLDS
  | Epers_FirstCommunion
  | Epers_Funeral
  | Epers_Graduate
  | Epers_Hospitalisation
  | Epers_Illness
  | Epers_Immigration
  | Epers_ListePassenger
  | Epers_MilitaryDistinction
  | Epers_MilitaryPromotion
  | Epers_MilitaryService
  | Epers_MobilisationMilitaire
  | Epers_Naturalisation
  | Epers_Occupation
  | Epers_Ordination
  | Epers_Property
  | Epers_Recensement
  | Epers_Residence
  | Epers_Retired
  | Epers_ScellentChildLDS
  | Epers_ScellentParentLDS
  | Epers_ScellentSpouseLDS
  | Epers_VenteBien
  | Epers_Will
  | Epers_Name of 'string
type ('person, 'string) gen_pers_event =
  { epers_name : 'string gen_pers_event_name;
    epers_date : cdate;
    epers_place : 'string;
    epers_reason : 'string;
    epers_note : 'string;
    epers_src : 'string;
    epers_witnesses : ('person * witness_kind) array }

type 'string gen_fam_event_name =
    Efam_Marriage
  | Efam_NoMarriage
  | Efam_NoMention
  | Efam_Engage
  | Efam_Divorce
  | Efam_Separated
  | Efam_Annulation
  | Efam_MarriageBann
  | Efam_MarriageContract
  | Efam_MarriageLicense
  | Efam_PACS
  | Efam_Residence
  | Efam_Name of 'string
type ('person, 'string) gen_fam_event =
  { efam_name : 'string gen_fam_event_name;
    efam_date : cdate;
    efam_place : 'string;
    efam_reason : 'string;
    efam_note : 'string;
    efam_src : 'string;
    efam_witnesses : ('person * witness_kind) array }


type relation_type =
  Adoption | Recognition | CandidateParent | GodParent | FosterParent

type ('person, 'string) gen_relation =
  { r_type : relation_type;
    r_fath : 'person option;
    r_moth : 'person option;
    r_sources : 'string }

type sex = Male | Female | Neuter

type place =
  { other : string;
    town : string;
    township : string;
    canton : string;
    district : string;
    county : string;
    region : string;
    country : string }

(* person *)

type ('person, 'string) gen_person =
  { first_name : 'string;
    surname : 'string;
    occ : int;
    image : 'string;
    public_name : 'string;
    qualifiers : 'string list;
    aliases : 'string list;
    first_names_aliases : 'string list;
    surnames_aliases : 'string list;
    titles : 'string gen_title list;
    rparents : ('person, 'string) gen_relation list;
    related : iper list;
    occupation : 'string;
    sex : sex;
    access : access;
    birth : cdate;
    birth_place : 'string;
    birth_note : 'string;
    birth_src : 'string;
    baptism : cdate;
    baptism_place : 'string;
    baptism_note : 'string;
    baptism_src : 'string;
    death : death;
    death_place : 'string;
    death_note : 'string;
    death_src : 'string;
    burial : burial;
    burial_place : 'string;
    burial_note : 'string;
    burial_src : 'string;
    pevents : ('person, 'string) gen_pers_event list;
    notes : 'string;
    psources : 'string;
    key_index : iper }


type 'family gen_ascend = { parents : 'family option; consang : Adef.fix }

type 'family gen_union = { family : 'family array }

(* family *)

type ('person, 'string) gen_family =
  { marriage : cdate;
    marriage_place : 'string;
    marriage_note : 'string;
    marriage_src : 'string;
    witnesses : 'person array;
    relation : relation_kind;
    divorce : divorce;
    fevents : ('person, 'string) gen_fam_event list;
    comment : 'string;
    origin_file : 'string;
    fsources : 'string;
    fam_index : ifam }

type 'person gen_couple = 'person Adef.gen_couple

type 'person gen_descend = { children : 'person array }

type 'person error =
    AlreadyDefined of 'person
  | OwnAncestor of 'person
  | BadSexOfMarriedPerson of 'person

type ('person, 'descend, 'title, 'pevent, 'fevent) warning =
    BigAgeBetweenSpouses of 'person * 'person * dmy
  | BirthAfterDeath of 'person
  | IncoherentSex of 'person * int * int
  | ChangedOrderOfChildren of ifam * 'descend * iper array * iper array
  | ChangedOrderOfMarriages of 'person * ifam array * ifam array
  | ChangedOrderOfFamilyEvents of ifam * 'fevent list * 'fevent list
  | ChangedOrderOfPersonEvents of 'person * 'pevent list * 'pevent list
  | ChildrenNotInOrder of ifam * 'descend * 'person * 'person
  | CloseChildren of ifam * 'descend * 'person * 'person
  | DeadOld of 'person * dmy
  | DeadTooEarlyToBeFather of 'person * 'person
  | FEventOrder of 'person * 'fevent * 'fevent
  | FWitnessEventAfterDeath of 'person * 'fevent
  | FWitnessEventBeforeBirth of 'person * 'fevent
  | IncoherentAncestorDate of 'person * 'person
  | MarriageDateAfterDeath of 'person
  | MarriageDateBeforeBirth of 'person
  | MotherDeadAfterChildBirth of 'person * 'person
  | ParentBornAfterChild of 'person * 'person
  | ParentTooOld of 'person * dmy
  | ParentTooYoung of 'person * dmy
  | PEventOrder of 'person * 'pevent * 'pevent
  | PossibleDuplicateFam of ifam * ifam
  | PWitnessEventAfterDeath of 'person * 'pevent
  | PWitnessEventBeforeBirth of 'person * 'pevent
  | TitleDatesError of 'person * 'title
  | UndefinedSex of 'person
  | WitnessDateAfterDeath of 'person
  | WitnessDateBeforeBirth of 'person
  | YoungForMarriage of 'person * dmy

type ('person, 'descend, 'title) misc = MissingSources

type rn_mode = RnAll | Rn1Ln | RnDeg


(* Historique des modifications *)

type ('person, 'string) base_changed =
    U_Add_person of ('person, 'string) gen_person
  | U_Modify_person of
      ('person, 'string) gen_person * ('person, 'string) gen_person
  | U_Delete_person of ('person, 'string) gen_person
  | U_Merge_person of
      ('person, 'string) gen_person * ('person, 'string) gen_person *
        ('person, 'string) gen_person
  | U_Send_image of ('person, 'string) gen_person
  | U_Delete_image of ('person, 'string) gen_person
  | U_Add_family of
      ('person, 'string) gen_person * ('person, 'string) gen_family
  | U_Modify_family of
      ('person, 'string) gen_person * ('person, 'string) gen_family *
        ('person, 'string) gen_family
  | U_Delete_family of
      ('person, 'string) gen_person * ('person, 'string) gen_family
  | U_Invert_family of ('person, 'string) gen_person * ifam
  | U_Merge_family of
      ('person, 'string) gen_person * ('person, 'string) gen_family *
        ('person, 'string) gen_family * ('person, 'string) gen_family
  | U_Change_children_name of
      ('person, 'string) gen_person *
        ((string * string * int * iper) * (string * string * int * iper)) list
  | U_Add_parent of
      ('person, 'string) gen_person * ('person, 'string) gen_family
  | U_Kill_ancestors of ('person, 'string) gen_person
  | U_Multi of
      ('person, 'string) gen_person * ('person, 'string) gen_person * bool
  | U_Notes of int option * string
