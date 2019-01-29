<?php
// Please include the below file before api.proto.php
//require('protocolbuffers.inc.php');
namespace geneweb\api\object {
  // message geneweb.api.object.InfosBase
  class InfosBase {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\InfosBase: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->nbPersons_ = $tmp;
            
            break;
          case 2:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->nbFamilies_ = $tmp;
            
            break;
          case 3:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->sosa_ = new \geneweb\api\object\ReferencePerson($fp, $len);
            ASSERT('$len == 0');
            break;
          case 4:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->lastModifiedPerson_ = $tmp;
            
            break;
          case 5:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->realNbPersons_ = $tmp;
            
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->nbPersons_)) {
        fwrite($fp, "\x08");
        \Protobuf::write_varint($fp, $this->nbPersons_);
      }
      if (!is_null($this->nbFamilies_)) {
        fwrite($fp, "\x10");
        \Protobuf::write_varint($fp, $this->nbFamilies_);
      }
      if (!is_null($this->sosa_)) {
        fwrite($fp, "\x1a");
        \Protobuf::write_varint($fp, $this->sosa_->size()); // message
        $this->sosa_->write($fp);
      }
      if (!is_null($this->lastModifiedPerson_)) {
        fwrite($fp, " ");
        \Protobuf::write_varint($fp, $this->lastModifiedPerson_);
      }
      if (!is_null($this->realNbPersons_)) {
        fwrite($fp, "(");
        \Protobuf::write_varint($fp, $this->realNbPersons_);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->nbPersons_)) {
        $size += 1 + \Protobuf::size_varint($this->nbPersons_);
      }
      if (!is_null($this->nbFamilies_)) {
        $size += 1 + \Protobuf::size_varint($this->nbFamilies_);
      }
      if (!is_null($this->sosa_)) {
        $l = $this->sosa_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->lastModifiedPerson_)) {
        $size += 1 + \Protobuf::size_varint($this->lastModifiedPerson_);
      }
      if (!is_null($this->realNbPersons_)) {
        $size += 1 + \Protobuf::size_varint($this->realNbPersons_);
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->nbPersons_ === null) return false;
      if ($this->nbFamilies_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('nbPersons_', $this->nbPersons_)
           . \Protobuf::toString('nbFamilies_', $this->nbFamilies_)
           . \Protobuf::toString('sosa_', $this->sosa_)
           . \Protobuf::toString('lastModifiedPerson_', $this->lastModifiedPerson_)
           . \Protobuf::toString('realNbPersons_', $this->realNbPersons_);
    }
    
    // required int64 nb_persons = 1;

    private $nbPersons_ = null;
    public function clearNbPersons() { $this->nbPersons_ = null; }
    public function hasNbPersons() { return $this->nbPersons_ !== null; }
    public function getNbPersons() { if($this->nbPersons_ === null) return 0; else return $this->nbPersons_; }
    public function setNbPersons($value) { $this->nbPersons_ = $value; }
    
    // required int64 nb_families = 2;

    private $nbFamilies_ = null;
    public function clearNbFamilies() { $this->nbFamilies_ = null; }
    public function hasNbFamilies() { return $this->nbFamilies_ !== null; }
    public function getNbFamilies() { if($this->nbFamilies_ === null) return 0; else return $this->nbFamilies_; }
    public function setNbFamilies($value) { $this->nbFamilies_ = $value; }
    
    // optional .geneweb.api.object.ReferencePerson sosa = 3;

    private $sosa_ = null;
    public function clearSosa() { $this->sosa_ = null; }
    public function hasSosa() { return $this->sosa_ !== null; }
    public function getSosa() { if($this->sosa_ === null) return null; else return $this->sosa_; }
    public function setSosa(\geneweb\api\object\ReferencePerson $value) { $this->sosa_ = $value; }
    
    // optional int64 last_modified_person = 4;

    private $lastModifiedPerson_ = null;
    public function clearLastModifiedPerson() { $this->lastModifiedPerson_ = null; }
    public function hasLastModifiedPerson() { return $this->lastModifiedPerson_ !== null; }
    public function getLastModifiedPerson() { if($this->lastModifiedPerson_ === null) return 0; else return $this->lastModifiedPerson_; }
    public function setLastModifiedPerson($value) { $this->lastModifiedPerson_ = $value; }
    
    // optional int64 real_nb_persons = 5;

    private $realNbPersons_ = null;
    public function clearRealNbPersons() { $this->realNbPersons_ = null; }
    public function hasRealNbPersons() { return $this->realNbPersons_ !== null; }
    public function getRealNbPersons() { if($this->realNbPersons_ === null) return 0; else return $this->realNbPersons_; }
    public function setRealNbPersons($value) { $this->realNbPersons_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.InfosBase)
  }
  
  // message geneweb.api.object.ReferencePerson
  class ReferencePerson {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\ReferencePerson: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->n_ = $tmp;
            $limit-=$len;
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->p_ = $tmp;
            $limit-=$len;
            break;
          case 3:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->oc_ = $tmp;
            
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->n_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, strlen($this->n_));
        fwrite($fp, $this->n_);
      }
      if (!is_null($this->p_)) {
        fwrite($fp, "\x12");
        \Protobuf::write_varint($fp, strlen($this->p_));
        fwrite($fp, $this->p_);
      }
      if (!is_null($this->oc_)) {
        fwrite($fp, "\x18");
        \Protobuf::write_varint($fp, $this->oc_);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->n_)) {
        $l = strlen($this->n_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->p_)) {
        $l = strlen($this->p_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->oc_)) {
        $size += 1 + \Protobuf::size_varint($this->oc_);
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->n_ === null) return false;
      if ($this->p_ === null) return false;
      if ($this->oc_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('n_', $this->n_)
           . \Protobuf::toString('p_', $this->p_)
           . \Protobuf::toString('oc_', $this->oc_);
    }
    
    // required string n = 1;

    private $n_ = null;
    public function clearN() { $this->n_ = null; }
    public function hasN() { return $this->n_ !== null; }
    public function getN() { if($this->n_ === null) return ""; else return $this->n_; }
    public function setN($value) { $this->n_ = $value; }
    
    // required string p = 2;

    private $p_ = null;
    public function clearP() { $this->p_ = null; }
    public function hasP() { return $this->p_ !== null; }
    public function getP() { if($this->p_ === null) return ""; else return $this->p_; }
    public function setP($value) { $this->p_ = $value; }
    
    // required int32 oc = 3;

    private $oc_ = null;
    public function clearOc() { $this->oc_ = null; }
    public function hasOc() { return $this->oc_ !== null; }
    public function getOc() { if($this->oc_ === null) return 0; else return $this->oc_; }
    public function setOc($value) { $this->oc_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.ReferencePerson)
  }
  
  // message geneweb.api.object.ListReferencePersons
  class ListReferencePersons {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\ListReferencePersons: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->listRefPersons_[] = new \geneweb\api\object\ReferencePerson($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->listRefPersons_))
        foreach($this->listRefPersons_ as $v) {
          fwrite($fp, "\x0a");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->listRefPersons_))
        foreach($this->listRefPersons_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      return $size;
    }
    
    public function validateRequired() {
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('listRefPersons_', $this->listRefPersons_);
    }
    
    // repeated .geneweb.api.object.ReferencePerson list_ref_persons = 1;

    private $listRefPersons_ = null;
    public function clearListRefPersons() { $this->listRefPersons_ = null; }
    public function getListRefPersonsCount() { if ($this->listRefPersons_ === null ) return 0; else return count($this->listRefPersons_); }
    public function getListRefPersons($index) { return $this->listRefPersons_[$index]; }
    public function getListRefPersonsArray() { if ($this->listRefPersons_ === null ) return array(); else return $this->listRefPersons_; }
    public function setListRefPersons($index, $value) {$this->listRefPersons_[$index] = $value;	}
    public function addListRefPersons($value) { $this->listRefPersons_[] = $value; }
    public function addAllListRefPersons(array $values) { foreach($values as $value) {$this->listRefPersons_[] = $value;} }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.ListReferencePersons)
  }
  
  // message geneweb.api.object.RelationParent
  class RelationParent {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\RelationParent: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->father_ = $tmp;
            
            break;
          case 2:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->mother_ = $tmp;
            
            break;
          case 3:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->source_ = $tmp;
            $limit-=$len;
            break;
          case 4:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->rptType_ = $tmp;
            
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->father_)) {
        fwrite($fp, "\x08");
        \Protobuf::write_varint($fp, $this->father_);
      }
      if (!is_null($this->mother_)) {
        fwrite($fp, "\x10");
        \Protobuf::write_varint($fp, $this->mother_);
      }
      if (!is_null($this->source_)) {
        fwrite($fp, "\x1a");
        \Protobuf::write_varint($fp, strlen($this->source_));
        fwrite($fp, $this->source_);
      }
      if (!is_null($this->rptType_)) {
        fwrite($fp, " ");
        \Protobuf::write_varint($fp, $this->rptType_);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->father_)) {
        $size += 1 + \Protobuf::size_varint($this->father_);
      }
      if (!is_null($this->mother_)) {
        $size += 1 + \Protobuf::size_varint($this->mother_);
      }
      if (!is_null($this->source_)) {
        $l = strlen($this->source_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->rptType_)) {
        $size += 1 + \Protobuf::size_varint($this->rptType_);
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->rptType_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('father_', $this->father_)
           . \Protobuf::toString('mother_', $this->mother_)
           . \Protobuf::toString('source_', $this->source_)
           . \Protobuf::toString('rptType_', \geneweb\api\object\RelationParentType::toString($this->rptType_));
    }
    
    // optional int32 father = 1;

    private $father_ = null;
    public function clearFather() { $this->father_ = null; }
    public function hasFather() { return $this->father_ !== null; }
    public function getFather() { if($this->father_ === null) return 0; else return $this->father_; }
    public function setFather($value) { $this->father_ = $value; }
    
    // optional int32 mother = 2;

    private $mother_ = null;
    public function clearMother() { $this->mother_ = null; }
    public function hasMother() { return $this->mother_ !== null; }
    public function getMother() { if($this->mother_ === null) return 0; else return $this->mother_; }
    public function setMother($value) { $this->mother_ = $value; }
    
    // optional string source = 3;

    private $source_ = null;
    public function clearSource() { $this->source_ = null; }
    public function hasSource() { return $this->source_ !== null; }
    public function getSource() { if($this->source_ === null) return ""; else return $this->source_; }
    public function setSource($value) { $this->source_ = $value; }
    
    // required .geneweb.api.object.RelationParentType rpt_type = 4;

    private $rptType_ = null;
    public function clearRptType() { $this->rptType_ = null; }
    public function hasRptType() { return $this->rptType_ !== null; }
    public function getRptType() { if($this->rptType_ === null) return \geneweb\api\object\RelationParentType::RPT_ADOPTION; else return $this->rptType_; }
    public function setRptType($value) { $this->rptType_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.RelationParent)
  }
  
  // message geneweb.api.object.Title
  class Title {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\Title: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->titleType_ = $tmp;
            
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->name_ = $tmp;
            $limit-=$len;
            break;
          case 3:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->title_ = $tmp;
            $limit-=$len;
            break;
          case 4:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->fief_ = $tmp;
            $limit-=$len;
            break;
          case 5:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->dateBegin_ = $tmp;
            $limit-=$len;
            break;
          case 6:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->dateEnd_ = $tmp;
            $limit-=$len;
            break;
          case 7:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->nth_ = $tmp;
            
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->titleType_)) {
        fwrite($fp, "\x08");
        \Protobuf::write_varint($fp, $this->titleType_);
      }
      if (!is_null($this->name_)) {
        fwrite($fp, "\x12");
        \Protobuf::write_varint($fp, strlen($this->name_));
        fwrite($fp, $this->name_);
      }
      if (!is_null($this->title_)) {
        fwrite($fp, "\x1a");
        \Protobuf::write_varint($fp, strlen($this->title_));
        fwrite($fp, $this->title_);
      }
      if (!is_null($this->fief_)) {
        fwrite($fp, "\"");
        \Protobuf::write_varint($fp, strlen($this->fief_));
        fwrite($fp, $this->fief_);
      }
      if (!is_null($this->dateBegin_)) {
        fwrite($fp, "*");
        \Protobuf::write_varint($fp, strlen($this->dateBegin_));
        fwrite($fp, $this->dateBegin_);
      }
      if (!is_null($this->dateEnd_)) {
        fwrite($fp, "2");
        \Protobuf::write_varint($fp, strlen($this->dateEnd_));
        fwrite($fp, $this->dateEnd_);
      }
      if (!is_null($this->nth_)) {
        fwrite($fp, "8");
        \Protobuf::write_varint($fp, $this->nth_);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->titleType_)) {
        $size += 1 + \Protobuf::size_varint($this->titleType_);
      }
      if (!is_null($this->name_)) {
        $l = strlen($this->name_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->title_)) {
        $l = strlen($this->title_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->fief_)) {
        $l = strlen($this->fief_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->dateBegin_)) {
        $l = strlen($this->dateBegin_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->dateEnd_)) {
        $l = strlen($this->dateEnd_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->nth_)) {
        $size += 1 + \Protobuf::size_varint($this->nth_);
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->titleType_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('titleType_', \geneweb\api\object\TitleType::toString($this->titleType_))
           . \Protobuf::toString('name_', $this->name_)
           . \Protobuf::toString('title_', $this->title_)
           . \Protobuf::toString('fief_', $this->fief_)
           . \Protobuf::toString('dateBegin_', $this->dateBegin_)
           . \Protobuf::toString('dateEnd_', $this->dateEnd_)
           . \Protobuf::toString('nth_', $this->nth_);
    }
    
    // required .geneweb.api.object.TitleType title_type = 1;

    private $titleType_ = null;
    public function clearTitleType() { $this->titleType_ = null; }
    public function hasTitleType() { return $this->titleType_ !== null; }
    public function getTitleType() { if($this->titleType_ === null) return \geneweb\api\object\TitleType::TITLE_MAIN; else return $this->titleType_; }
    public function setTitleType($value) { $this->titleType_ = $value; }
    
    // optional string name = 2;

    private $name_ = null;
    public function clearName() { $this->name_ = null; }
    public function hasName() { return $this->name_ !== null; }
    public function getName() { if($this->name_ === null) return ""; else return $this->name_; }
    public function setName($value) { $this->name_ = $value; }
    
    // optional string title = 3;

    private $title_ = null;
    public function clearTitle() { $this->title_ = null; }
    public function hasTitle() { return $this->title_ !== null; }
    public function getTitle() { if($this->title_ === null) return ""; else return $this->title_; }
    public function setTitle($value) { $this->title_ = $value; }
    
    // optional string fief = 4;

    private $fief_ = null;
    public function clearFief() { $this->fief_ = null; }
    public function hasFief() { return $this->fief_ !== null; }
    public function getFief() { if($this->fief_ === null) return ""; else return $this->fief_; }
    public function setFief($value) { $this->fief_ = $value; }
    
    // optional string date_begin = 5;

    private $dateBegin_ = null;
    public function clearDateBegin() { $this->dateBegin_ = null; }
    public function hasDateBegin() { return $this->dateBegin_ !== null; }
    public function getDateBegin() { if($this->dateBegin_ === null) return ""; else return $this->dateBegin_; }
    public function setDateBegin($value) { $this->dateBegin_ = $value; }
    
    // optional string date_end = 6;

    private $dateEnd_ = null;
    public function clearDateEnd() { $this->dateEnd_ = null; }
    public function hasDateEnd() { return $this->dateEnd_ !== null; }
    public function getDateEnd() { if($this->dateEnd_ === null) return ""; else return $this->dateEnd_; }
    public function setDateEnd($value) { $this->dateEnd_ = $value; }
    
    // optional int32 nth = 7;

    private $nth_ = null;
    public function clearNth() { $this->nth_ = null; }
    public function hasNth() { return $this->nth_ !== null; }
    public function getNth() { if($this->nth_ === null) return 0; else return $this->nth_; }
    public function setNth($value) { $this->nth_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.Title)
  }
  
  // message geneweb.api.object.Spouse
  class Spouse {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\Spouse: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->sosa_ = $tmp;
            $limit-=$len;
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->n_ = $tmp;
            $limit-=$len;
            break;
          case 3:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->p_ = $tmp;
            $limit-=$len;
            break;
          case 4:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->oc_ = $tmp;
            
            break;
          case 5:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->sex_ = $tmp;
            
            break;
          case 6:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->lastname_ = $tmp;
            $limit-=$len;
            break;
          case 7:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->firstname_ = $tmp;
            $limit-=$len;
            break;
          case 8:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->publicName_ = $tmp;
            $limit-=$len;
            break;
          case 9:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->image_ = $tmp;
            $limit-=$len;
            break;
          case 10:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->birthDate_ = $tmp;
            $limit-=$len;
            break;
          case 11:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->birthPlace_ = $tmp;
            $limit-=$len;
            break;
          case 12:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->baptismDate_ = $tmp;
            $limit-=$len;
            break;
          case 13:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->baptismPlace_ = $tmp;
            $limit-=$len;
            break;
          case 14:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->deathDate_ = $tmp;
            $limit-=$len;
            break;
          case 15:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->deathPlace_ = $tmp;
            $limit-=$len;
            break;
          case 16:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->deathType_ = $tmp;
            
            break;
          case 17:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->burialDate_ = $tmp;
            $limit-=$len;
            break;
          case 18:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->burialPlace_ = $tmp;
            $limit-=$len;
            break;
          case 19:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->marriageDate_ = $tmp;
            $limit-=$len;
            break;
          case 20:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->marriagePlace_ = $tmp;
            $limit-=$len;
            break;
          case 21:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->divorceType_ = $tmp;
            
            break;
          case 22:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->visibleForVisitors_ = $tmp > 0 ? true : false;
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->sosa_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, strlen($this->sosa_));
        fwrite($fp, $this->sosa_);
      }
      if (!is_null($this->n_)) {
        fwrite($fp, "\x12");
        \Protobuf::write_varint($fp, strlen($this->n_));
        fwrite($fp, $this->n_);
      }
      if (!is_null($this->p_)) {
        fwrite($fp, "\x1a");
        \Protobuf::write_varint($fp, strlen($this->p_));
        fwrite($fp, $this->p_);
      }
      if (!is_null($this->oc_)) {
        fwrite($fp, " ");
        \Protobuf::write_varint($fp, $this->oc_);
      }
      if (!is_null($this->sex_)) {
        fwrite($fp, "(");
        \Protobuf::write_varint($fp, $this->sex_);
      }
      if (!is_null($this->lastname_)) {
        fwrite($fp, "2");
        \Protobuf::write_varint($fp, strlen($this->lastname_));
        fwrite($fp, $this->lastname_);
      }
      if (!is_null($this->firstname_)) {
        fwrite($fp, ":");
        \Protobuf::write_varint($fp, strlen($this->firstname_));
        fwrite($fp, $this->firstname_);
      }
      if (!is_null($this->publicName_)) {
        fwrite($fp, "B");
        \Protobuf::write_varint($fp, strlen($this->publicName_));
        fwrite($fp, $this->publicName_);
      }
      if (!is_null($this->image_)) {
        fwrite($fp, "J");
        \Protobuf::write_varint($fp, strlen($this->image_));
        fwrite($fp, $this->image_);
      }
      if (!is_null($this->birthDate_)) {
        fwrite($fp, "R");
        \Protobuf::write_varint($fp, strlen($this->birthDate_));
        fwrite($fp, $this->birthDate_);
      }
      if (!is_null($this->birthPlace_)) {
        fwrite($fp, "Z");
        \Protobuf::write_varint($fp, strlen($this->birthPlace_));
        fwrite($fp, $this->birthPlace_);
      }
      if (!is_null($this->baptismDate_)) {
        fwrite($fp, "b");
        \Protobuf::write_varint($fp, strlen($this->baptismDate_));
        fwrite($fp, $this->baptismDate_);
      }
      if (!is_null($this->baptismPlace_)) {
        fwrite($fp, "j");
        \Protobuf::write_varint($fp, strlen($this->baptismPlace_));
        fwrite($fp, $this->baptismPlace_);
      }
      if (!is_null($this->deathDate_)) {
        fwrite($fp, "r");
        \Protobuf::write_varint($fp, strlen($this->deathDate_));
        fwrite($fp, $this->deathDate_);
      }
      if (!is_null($this->deathPlace_)) {
        fwrite($fp, "z");
        \Protobuf::write_varint($fp, strlen($this->deathPlace_));
        fwrite($fp, $this->deathPlace_);
      }
      if (!is_null($this->deathType_)) {
        fwrite($fp, "\x80\x01");
        \Protobuf::write_varint($fp, $this->deathType_);
      }
      if (!is_null($this->burialDate_)) {
        fwrite($fp, "\x8a\x01");
        \Protobuf::write_varint($fp, strlen($this->burialDate_));
        fwrite($fp, $this->burialDate_);
      }
      if (!is_null($this->burialPlace_)) {
        fwrite($fp, "\x92\x01");
        \Protobuf::write_varint($fp, strlen($this->burialPlace_));
        fwrite($fp, $this->burialPlace_);
      }
      if (!is_null($this->marriageDate_)) {
        fwrite($fp, "\x9a\x01");
        \Protobuf::write_varint($fp, strlen($this->marriageDate_));
        fwrite($fp, $this->marriageDate_);
      }
      if (!is_null($this->marriagePlace_)) {
        fwrite($fp, "\xa2\x01");
        \Protobuf::write_varint($fp, strlen($this->marriagePlace_));
        fwrite($fp, $this->marriagePlace_);
      }
      if (!is_null($this->divorceType_)) {
        fwrite($fp, "\xa8\x01");
        \Protobuf::write_varint($fp, $this->divorceType_);
      }
      if (!is_null($this->visibleForVisitors_)) {
        fwrite($fp, "\xb0\x01");
        \Protobuf::write_varint($fp, $this->visibleForVisitors_ ? 1 : 0);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->sosa_)) {
        $l = strlen($this->sosa_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->n_)) {
        $l = strlen($this->n_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->p_)) {
        $l = strlen($this->p_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->oc_)) {
        $size += 1 + \Protobuf::size_varint($this->oc_);
      }
      if (!is_null($this->sex_)) {
        $size += 1 + \Protobuf::size_varint($this->sex_);
      }
      if (!is_null($this->lastname_)) {
        $l = strlen($this->lastname_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->firstname_)) {
        $l = strlen($this->firstname_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->publicName_)) {
        $l = strlen($this->publicName_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->image_)) {
        $l = strlen($this->image_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->birthDate_)) {
        $l = strlen($this->birthDate_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->birthPlace_)) {
        $l = strlen($this->birthPlace_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->baptismDate_)) {
        $l = strlen($this->baptismDate_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->baptismPlace_)) {
        $l = strlen($this->baptismPlace_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->deathDate_)) {
        $l = strlen($this->deathDate_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->deathPlace_)) {
        $l = strlen($this->deathPlace_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->deathType_)) {
        $size += 2 + \Protobuf::size_varint($this->deathType_);
      }
      if (!is_null($this->burialDate_)) {
        $l = strlen($this->burialDate_);
        $size += 2 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->burialPlace_)) {
        $l = strlen($this->burialPlace_);
        $size += 2 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->marriageDate_)) {
        $l = strlen($this->marriageDate_);
        $size += 2 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->marriagePlace_)) {
        $l = strlen($this->marriagePlace_);
        $size += 2 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->divorceType_)) {
        $size += 2 + \Protobuf::size_varint($this->divorceType_);
      }
      if (!is_null($this->visibleForVisitors_)) {
        $size += 3;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->sosa_ === null) return false;
      if ($this->n_ === null) return false;
      if ($this->p_ === null) return false;
      if ($this->oc_ === null) return false;
      if ($this->sex_ === null) return false;
      if ($this->lastname_ === null) return false;
      if ($this->firstname_ === null) return false;
      if ($this->image_ === null) return false;
      if ($this->birthDate_ === null) return false;
      if ($this->birthPlace_ === null) return false;
      if ($this->baptismDate_ === null) return false;
      if ($this->baptismPlace_ === null) return false;
      if ($this->deathDate_ === null) return false;
      if ($this->deathPlace_ === null) return false;
      if ($this->deathType_ === null) return false;
      if ($this->burialDate_ === null) return false;
      if ($this->burialPlace_ === null) return false;
      if ($this->marriageDate_ === null) return false;
      if ($this->marriagePlace_ === null) return false;
      if ($this->divorceType_ === null) return false;
      if ($this->visibleForVisitors_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('sosa_', $this->sosa_)
           . \Protobuf::toString('n_', $this->n_)
           . \Protobuf::toString('p_', $this->p_)
           . \Protobuf::toString('oc_', $this->oc_)
           . \Protobuf::toString('sex_', \geneweb\api\object\Sex::toString($this->sex_))
           . \Protobuf::toString('lastname_', $this->lastname_)
           . \Protobuf::toString('firstname_', $this->firstname_)
           . \Protobuf::toString('publicName_', $this->publicName_)
           . \Protobuf::toString('image_', $this->image_)
           . \Protobuf::toString('birthDate_', $this->birthDate_)
           . \Protobuf::toString('birthPlace_', $this->birthPlace_)
           . \Protobuf::toString('baptismDate_', $this->baptismDate_)
           . \Protobuf::toString('baptismPlace_', $this->baptismPlace_)
           . \Protobuf::toString('deathDate_', $this->deathDate_)
           . \Protobuf::toString('deathPlace_', $this->deathPlace_)
           . \Protobuf::toString('deathType_', \geneweb\api\object\DeathType::toString($this->deathType_))
           . \Protobuf::toString('burialDate_', $this->burialDate_)
           . \Protobuf::toString('burialPlace_', $this->burialPlace_)
           . \Protobuf::toString('marriageDate_', $this->marriageDate_)
           . \Protobuf::toString('marriagePlace_', $this->marriagePlace_)
           . \Protobuf::toString('divorceType_', \geneweb\api\object\DivorceType::toString($this->divorceType_))
           . \Protobuf::toString('visibleForVisitors_', $this->visibleForVisitors_);
    }
    
    // required string sosa = 1;

    private $sosa_ = null;
    public function clearSosa() { $this->sosa_ = null; }
    public function hasSosa() { return $this->sosa_ !== null; }
    public function getSosa() { if($this->sosa_ === null) return ""; else return $this->sosa_; }
    public function setSosa($value) { $this->sosa_ = $value; }
    
    // required string n = 2;

    private $n_ = null;
    public function clearN() { $this->n_ = null; }
    public function hasN() { return $this->n_ !== null; }
    public function getN() { if($this->n_ === null) return ""; else return $this->n_; }
    public function setN($value) { $this->n_ = $value; }
    
    // required string p = 3;

    private $p_ = null;
    public function clearP() { $this->p_ = null; }
    public function hasP() { return $this->p_ !== null; }
    public function getP() { if($this->p_ === null) return ""; else return $this->p_; }
    public function setP($value) { $this->p_ = $value; }
    
    // required int32 oc = 4;

    private $oc_ = null;
    public function clearOc() { $this->oc_ = null; }
    public function hasOc() { return $this->oc_ !== null; }
    public function getOc() { if($this->oc_ === null) return 0; else return $this->oc_; }
    public function setOc($value) { $this->oc_ = $value; }
    
    // required .geneweb.api.object.Sex sex = 5;

    private $sex_ = null;
    public function clearSex() { $this->sex_ = null; }
    public function hasSex() { return $this->sex_ !== null; }
    public function getSex() { if($this->sex_ === null) return \geneweb\api\object\Sex::MALE; else return $this->sex_; }
    public function setSex($value) { $this->sex_ = $value; }
    
    // required string lastname = 6;

    private $lastname_ = null;
    public function clearLastname() { $this->lastname_ = null; }
    public function hasLastname() { return $this->lastname_ !== null; }
    public function getLastname() { if($this->lastname_ === null) return ""; else return $this->lastname_; }
    public function setLastname($value) { $this->lastname_ = $value; }
    
    // required string firstname = 7;

    private $firstname_ = null;
    public function clearFirstname() { $this->firstname_ = null; }
    public function hasFirstname() { return $this->firstname_ !== null; }
    public function getFirstname() { if($this->firstname_ === null) return ""; else return $this->firstname_; }
    public function setFirstname($value) { $this->firstname_ = $value; }
    
    // optional string public_name = 8;

    private $publicName_ = null;
    public function clearPublicName() { $this->publicName_ = null; }
    public function hasPublicName() { return $this->publicName_ !== null; }
    public function getPublicName() { if($this->publicName_ === null) return ""; else return $this->publicName_; }
    public function setPublicName($value) { $this->publicName_ = $value; }
    
    // required string image = 9;

    private $image_ = null;
    public function clearImage() { $this->image_ = null; }
    public function hasImage() { return $this->image_ !== null; }
    public function getImage() { if($this->image_ === null) return ""; else return $this->image_; }
    public function setImage($value) { $this->image_ = $value; }
    
    // required string birth_date = 10;

    private $birthDate_ = null;
    public function clearBirthDate() { $this->birthDate_ = null; }
    public function hasBirthDate() { return $this->birthDate_ !== null; }
    public function getBirthDate() { if($this->birthDate_ === null) return ""; else return $this->birthDate_; }
    public function setBirthDate($value) { $this->birthDate_ = $value; }
    
    // required string birth_place = 11;

    private $birthPlace_ = null;
    public function clearBirthPlace() { $this->birthPlace_ = null; }
    public function hasBirthPlace() { return $this->birthPlace_ !== null; }
    public function getBirthPlace() { if($this->birthPlace_ === null) return ""; else return $this->birthPlace_; }
    public function setBirthPlace($value) { $this->birthPlace_ = $value; }
    
    // required string baptism_date = 12;

    private $baptismDate_ = null;
    public function clearBaptismDate() { $this->baptismDate_ = null; }
    public function hasBaptismDate() { return $this->baptismDate_ !== null; }
    public function getBaptismDate() { if($this->baptismDate_ === null) return ""; else return $this->baptismDate_; }
    public function setBaptismDate($value) { $this->baptismDate_ = $value; }
    
    // required string baptism_place = 13;

    private $baptismPlace_ = null;
    public function clearBaptismPlace() { $this->baptismPlace_ = null; }
    public function hasBaptismPlace() { return $this->baptismPlace_ !== null; }
    public function getBaptismPlace() { if($this->baptismPlace_ === null) return ""; else return $this->baptismPlace_; }
    public function setBaptismPlace($value) { $this->baptismPlace_ = $value; }
    
    // required string death_date = 14;

    private $deathDate_ = null;
    public function clearDeathDate() { $this->deathDate_ = null; }
    public function hasDeathDate() { return $this->deathDate_ !== null; }
    public function getDeathDate() { if($this->deathDate_ === null) return ""; else return $this->deathDate_; }
    public function setDeathDate($value) { $this->deathDate_ = $value; }
    
    // required string death_place = 15;

    private $deathPlace_ = null;
    public function clearDeathPlace() { $this->deathPlace_ = null; }
    public function hasDeathPlace() { return $this->deathPlace_ !== null; }
    public function getDeathPlace() { if($this->deathPlace_ === null) return ""; else return $this->deathPlace_; }
    public function setDeathPlace($value) { $this->deathPlace_ = $value; }
    
    // required .geneweb.api.object.DeathType death_type = 16;

    private $deathType_ = null;
    public function clearDeathType() { $this->deathType_ = null; }
    public function hasDeathType() { return $this->deathType_ !== null; }
    public function getDeathType() { if($this->deathType_ === null) return \geneweb\api\object\DeathType::NOT_DEAD; else return $this->deathType_; }
    public function setDeathType($value) { $this->deathType_ = $value; }
    
    // required string burial_date = 17;

    private $burialDate_ = null;
    public function clearBurialDate() { $this->burialDate_ = null; }
    public function hasBurialDate() { return $this->burialDate_ !== null; }
    public function getBurialDate() { if($this->burialDate_ === null) return ""; else return $this->burialDate_; }
    public function setBurialDate($value) { $this->burialDate_ = $value; }
    
    // required string burial_place = 18;

    private $burialPlace_ = null;
    public function clearBurialPlace() { $this->burialPlace_ = null; }
    public function hasBurialPlace() { return $this->burialPlace_ !== null; }
    public function getBurialPlace() { if($this->burialPlace_ === null) return ""; else return $this->burialPlace_; }
    public function setBurialPlace($value) { $this->burialPlace_ = $value; }
    
    // required string marriage_date = 19;

    private $marriageDate_ = null;
    public function clearMarriageDate() { $this->marriageDate_ = null; }
    public function hasMarriageDate() { return $this->marriageDate_ !== null; }
    public function getMarriageDate() { if($this->marriageDate_ === null) return ""; else return $this->marriageDate_; }
    public function setMarriageDate($value) { $this->marriageDate_ = $value; }
    
    // required string marriage_place = 20;

    private $marriagePlace_ = null;
    public function clearMarriagePlace() { $this->marriagePlace_ = null; }
    public function hasMarriagePlace() { return $this->marriagePlace_ !== null; }
    public function getMarriagePlace() { if($this->marriagePlace_ === null) return ""; else return $this->marriagePlace_; }
    public function setMarriagePlace($value) { $this->marriagePlace_ = $value; }
    
    // required .geneweb.api.object.DivorceType divorce_type = 21;

    private $divorceType_ = null;
    public function clearDivorceType() { $this->divorceType_ = null; }
    public function hasDivorceType() { return $this->divorceType_ !== null; }
    public function getDivorceType() { if($this->divorceType_ === null) return \geneweb\api\object\DivorceType::NOT_DIVORCED; else return $this->divorceType_; }
    public function setDivorceType($value) { $this->divorceType_ = $value; }
    
    // required bool visible_for_visitors = 22;

    private $visibleForVisitors_ = null;
    public function clearVisibleForVisitors() { $this->visibleForVisitors_ = null; }
    public function hasVisibleForVisitors() { return $this->visibleForVisitors_ !== null; }
    public function getVisibleForVisitors() { if($this->visibleForVisitors_ === null) return false; else return $this->visibleForVisitors_; }
    public function setVisibleForVisitors($value) { $this->visibleForVisitors_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.Spouse)
  }
  
  // message geneweb.api.object.Person
  class Person {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\Person: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->sosa_ = $tmp;
            $limit-=$len;
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->n_ = $tmp;
            $limit-=$len;
            break;
          case 3:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->p_ = $tmp;
            $limit-=$len;
            break;
          case 4:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->oc_ = $tmp;
            
            break;
          case 5:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->sex_ = $tmp;
            
            break;
          case 6:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->lastname_ = $tmp;
            $limit-=$len;
            break;
          case 7:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->firstname_ = $tmp;
            $limit-=$len;
            break;
          case 8:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->publicName_ = $tmp;
            $limit-=$len;
            break;
          case 9:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->image_ = $tmp;
            $limit-=$len;
            break;
          case 10:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->birthDate_ = $tmp;
            $limit-=$len;
            break;
          case 11:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->birthPlace_ = $tmp;
            $limit-=$len;
            break;
          case 12:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->baptismDate_ = $tmp;
            $limit-=$len;
            break;
          case 13:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->baptismPlace_ = $tmp;
            $limit-=$len;
            break;
          case 14:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->deathDate_ = $tmp;
            $limit-=$len;
            break;
          case 15:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->deathPlace_ = $tmp;
            $limit-=$len;
            break;
          case 16:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->deathType_ = $tmp;
            
            break;
          case 17:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->burialDate_ = $tmp;
            $limit-=$len;
            break;
          case 18:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->burialPlace_ = $tmp;
            $limit-=$len;
            break;
          case 19:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->spouses_[] = new \geneweb\api\object\Spouse($fp, $len);
            ASSERT('$len == 0');
            break;
          case 20:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->ascend_ = $tmp > 0 ? true : false;
            break;
          case 21:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->descend_ = $tmp > 0 ? true : false;
            break;
          case 22:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->visibleForVisitors_ = $tmp > 0 ? true : false;
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->sosa_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, strlen($this->sosa_));
        fwrite($fp, $this->sosa_);
      }
      if (!is_null($this->n_)) {
        fwrite($fp, "\x12");
        \Protobuf::write_varint($fp, strlen($this->n_));
        fwrite($fp, $this->n_);
      }
      if (!is_null($this->p_)) {
        fwrite($fp, "\x1a");
        \Protobuf::write_varint($fp, strlen($this->p_));
        fwrite($fp, $this->p_);
      }
      if (!is_null($this->oc_)) {
        fwrite($fp, " ");
        \Protobuf::write_varint($fp, $this->oc_);
      }
      if (!is_null($this->sex_)) {
        fwrite($fp, "(");
        \Protobuf::write_varint($fp, $this->sex_);
      }
      if (!is_null($this->lastname_)) {
        fwrite($fp, "2");
        \Protobuf::write_varint($fp, strlen($this->lastname_));
        fwrite($fp, $this->lastname_);
      }
      if (!is_null($this->firstname_)) {
        fwrite($fp, ":");
        \Protobuf::write_varint($fp, strlen($this->firstname_));
        fwrite($fp, $this->firstname_);
      }
      if (!is_null($this->publicName_)) {
        fwrite($fp, "B");
        \Protobuf::write_varint($fp, strlen($this->publicName_));
        fwrite($fp, $this->publicName_);
      }
      if (!is_null($this->image_)) {
        fwrite($fp, "J");
        \Protobuf::write_varint($fp, strlen($this->image_));
        fwrite($fp, $this->image_);
      }
      if (!is_null($this->birthDate_)) {
        fwrite($fp, "R");
        \Protobuf::write_varint($fp, strlen($this->birthDate_));
        fwrite($fp, $this->birthDate_);
      }
      if (!is_null($this->birthPlace_)) {
        fwrite($fp, "Z");
        \Protobuf::write_varint($fp, strlen($this->birthPlace_));
        fwrite($fp, $this->birthPlace_);
      }
      if (!is_null($this->baptismDate_)) {
        fwrite($fp, "b");
        \Protobuf::write_varint($fp, strlen($this->baptismDate_));
        fwrite($fp, $this->baptismDate_);
      }
      if (!is_null($this->baptismPlace_)) {
        fwrite($fp, "j");
        \Protobuf::write_varint($fp, strlen($this->baptismPlace_));
        fwrite($fp, $this->baptismPlace_);
      }
      if (!is_null($this->deathDate_)) {
        fwrite($fp, "r");
        \Protobuf::write_varint($fp, strlen($this->deathDate_));
        fwrite($fp, $this->deathDate_);
      }
      if (!is_null($this->deathPlace_)) {
        fwrite($fp, "z");
        \Protobuf::write_varint($fp, strlen($this->deathPlace_));
        fwrite($fp, $this->deathPlace_);
      }
      if (!is_null($this->deathType_)) {
        fwrite($fp, "\x80\x01");
        \Protobuf::write_varint($fp, $this->deathType_);
      }
      if (!is_null($this->burialDate_)) {
        fwrite($fp, "\x8a\x01");
        \Protobuf::write_varint($fp, strlen($this->burialDate_));
        fwrite($fp, $this->burialDate_);
      }
      if (!is_null($this->burialPlace_)) {
        fwrite($fp, "\x92\x01");
        \Protobuf::write_varint($fp, strlen($this->burialPlace_));
        fwrite($fp, $this->burialPlace_);
      }
      if (!is_null($this->spouses_))
        foreach($this->spouses_ as $v) {
          fwrite($fp, "\x9a\x01");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->ascend_)) {
        fwrite($fp, "\xa0\x01");
        \Protobuf::write_varint($fp, $this->ascend_ ? 1 : 0);
      }
      if (!is_null($this->descend_)) {
        fwrite($fp, "\xa8\x01");
        \Protobuf::write_varint($fp, $this->descend_ ? 1 : 0);
      }
      if (!is_null($this->visibleForVisitors_)) {
        fwrite($fp, "\xb0\x01");
        \Protobuf::write_varint($fp, $this->visibleForVisitors_ ? 1 : 0);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->sosa_)) {
        $l = strlen($this->sosa_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->n_)) {
        $l = strlen($this->n_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->p_)) {
        $l = strlen($this->p_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->oc_)) {
        $size += 1 + \Protobuf::size_varint($this->oc_);
      }
      if (!is_null($this->sex_)) {
        $size += 1 + \Protobuf::size_varint($this->sex_);
      }
      if (!is_null($this->lastname_)) {
        $l = strlen($this->lastname_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->firstname_)) {
        $l = strlen($this->firstname_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->publicName_)) {
        $l = strlen($this->publicName_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->image_)) {
        $l = strlen($this->image_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->birthDate_)) {
        $l = strlen($this->birthDate_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->birthPlace_)) {
        $l = strlen($this->birthPlace_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->baptismDate_)) {
        $l = strlen($this->baptismDate_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->baptismPlace_)) {
        $l = strlen($this->baptismPlace_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->deathDate_)) {
        $l = strlen($this->deathDate_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->deathPlace_)) {
        $l = strlen($this->deathPlace_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->deathType_)) {
        $size += 2 + \Protobuf::size_varint($this->deathType_);
      }
      if (!is_null($this->burialDate_)) {
        $l = strlen($this->burialDate_);
        $size += 2 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->burialPlace_)) {
        $l = strlen($this->burialPlace_);
        $size += 2 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->spouses_))
        foreach($this->spouses_ as $v) {
          $l = $v->size();
          $size += 2 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->ascend_)) {
        $size += 3;
      }
      if (!is_null($this->descend_)) {
        $size += 3;
      }
      if (!is_null($this->visibleForVisitors_)) {
        $size += 3;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->sosa_ === null) return false;
      if ($this->n_ === null) return false;
      if ($this->p_ === null) return false;
      if ($this->oc_ === null) return false;
      if ($this->sex_ === null) return false;
      if ($this->lastname_ === null) return false;
      if ($this->firstname_ === null) return false;
      if ($this->image_ === null) return false;
      if ($this->birthDate_ === null) return false;
      if ($this->birthPlace_ === null) return false;
      if ($this->baptismDate_ === null) return false;
      if ($this->baptismPlace_ === null) return false;
      if ($this->deathDate_ === null) return false;
      if ($this->deathPlace_ === null) return false;
      if ($this->deathType_ === null) return false;
      if ($this->burialDate_ === null) return false;
      if ($this->burialPlace_ === null) return false;
      if ($this->ascend_ === null) return false;
      if ($this->descend_ === null) return false;
      if ($this->visibleForVisitors_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('sosa_', $this->sosa_)
           . \Protobuf::toString('n_', $this->n_)
           . \Protobuf::toString('p_', $this->p_)
           . \Protobuf::toString('oc_', $this->oc_)
           . \Protobuf::toString('sex_', \geneweb\api\object\Sex::toString($this->sex_))
           . \Protobuf::toString('lastname_', $this->lastname_)
           . \Protobuf::toString('firstname_', $this->firstname_)
           . \Protobuf::toString('publicName_', $this->publicName_)
           . \Protobuf::toString('image_', $this->image_)
           . \Protobuf::toString('birthDate_', $this->birthDate_)
           . \Protobuf::toString('birthPlace_', $this->birthPlace_)
           . \Protobuf::toString('baptismDate_', $this->baptismDate_)
           . \Protobuf::toString('baptismPlace_', $this->baptismPlace_)
           . \Protobuf::toString('deathDate_', $this->deathDate_)
           . \Protobuf::toString('deathPlace_', $this->deathPlace_)
           . \Protobuf::toString('deathType_', \geneweb\api\object\DeathType::toString($this->deathType_))
           . \Protobuf::toString('burialDate_', $this->burialDate_)
           . \Protobuf::toString('burialPlace_', $this->burialPlace_)
           . \Protobuf::toString('spouses_', $this->spouses_)
           . \Protobuf::toString('ascend_', $this->ascend_)
           . \Protobuf::toString('descend_', $this->descend_)
           . \Protobuf::toString('visibleForVisitors_', $this->visibleForVisitors_);
    }
    
    // required string sosa = 1;

    private $sosa_ = null;
    public function clearSosa() { $this->sosa_ = null; }
    public function hasSosa() { return $this->sosa_ !== null; }
    public function getSosa() { if($this->sosa_ === null) return ""; else return $this->sosa_; }
    public function setSosa($value) { $this->sosa_ = $value; }
    
    // required string n = 2;

    private $n_ = null;
    public function clearN() { $this->n_ = null; }
    public function hasN() { return $this->n_ !== null; }
    public function getN() { if($this->n_ === null) return ""; else return $this->n_; }
    public function setN($value) { $this->n_ = $value; }
    
    // required string p = 3;

    private $p_ = null;
    public function clearP() { $this->p_ = null; }
    public function hasP() { return $this->p_ !== null; }
    public function getP() { if($this->p_ === null) return ""; else return $this->p_; }
    public function setP($value) { $this->p_ = $value; }
    
    // required int32 oc = 4;

    private $oc_ = null;
    public function clearOc() { $this->oc_ = null; }
    public function hasOc() { return $this->oc_ !== null; }
    public function getOc() { if($this->oc_ === null) return 0; else return $this->oc_; }
    public function setOc($value) { $this->oc_ = $value; }
    
    // required .geneweb.api.object.Sex sex = 5;

    private $sex_ = null;
    public function clearSex() { $this->sex_ = null; }
    public function hasSex() { return $this->sex_ !== null; }
    public function getSex() { if($this->sex_ === null) return \geneweb\api\object\Sex::MALE; else return $this->sex_; }
    public function setSex($value) { $this->sex_ = $value; }
    
    // required string lastname = 6;

    private $lastname_ = null;
    public function clearLastname() { $this->lastname_ = null; }
    public function hasLastname() { return $this->lastname_ !== null; }
    public function getLastname() { if($this->lastname_ === null) return ""; else return $this->lastname_; }
    public function setLastname($value) { $this->lastname_ = $value; }
    
    // required string firstname = 7;

    private $firstname_ = null;
    public function clearFirstname() { $this->firstname_ = null; }
    public function hasFirstname() { return $this->firstname_ !== null; }
    public function getFirstname() { if($this->firstname_ === null) return ""; else return $this->firstname_; }
    public function setFirstname($value) { $this->firstname_ = $value; }
    
    // optional string public_name = 8;

    private $publicName_ = null;
    public function clearPublicName() { $this->publicName_ = null; }
    public function hasPublicName() { return $this->publicName_ !== null; }
    public function getPublicName() { if($this->publicName_ === null) return ""; else return $this->publicName_; }
    public function setPublicName($value) { $this->publicName_ = $value; }
    
    // required string image = 9;

    private $image_ = null;
    public function clearImage() { $this->image_ = null; }
    public function hasImage() { return $this->image_ !== null; }
    public function getImage() { if($this->image_ === null) return ""; else return $this->image_; }
    public function setImage($value) { $this->image_ = $value; }
    
    // required string birth_date = 10;

    private $birthDate_ = null;
    public function clearBirthDate() { $this->birthDate_ = null; }
    public function hasBirthDate() { return $this->birthDate_ !== null; }
    public function getBirthDate() { if($this->birthDate_ === null) return ""; else return $this->birthDate_; }
    public function setBirthDate($value) { $this->birthDate_ = $value; }
    
    // required string birth_place = 11;

    private $birthPlace_ = null;
    public function clearBirthPlace() { $this->birthPlace_ = null; }
    public function hasBirthPlace() { return $this->birthPlace_ !== null; }
    public function getBirthPlace() { if($this->birthPlace_ === null) return ""; else return $this->birthPlace_; }
    public function setBirthPlace($value) { $this->birthPlace_ = $value; }
    
    // required string baptism_date = 12;

    private $baptismDate_ = null;
    public function clearBaptismDate() { $this->baptismDate_ = null; }
    public function hasBaptismDate() { return $this->baptismDate_ !== null; }
    public function getBaptismDate() { if($this->baptismDate_ === null) return ""; else return $this->baptismDate_; }
    public function setBaptismDate($value) { $this->baptismDate_ = $value; }
    
    // required string baptism_place = 13;

    private $baptismPlace_ = null;
    public function clearBaptismPlace() { $this->baptismPlace_ = null; }
    public function hasBaptismPlace() { return $this->baptismPlace_ !== null; }
    public function getBaptismPlace() { if($this->baptismPlace_ === null) return ""; else return $this->baptismPlace_; }
    public function setBaptismPlace($value) { $this->baptismPlace_ = $value; }
    
    // required string death_date = 14;

    private $deathDate_ = null;
    public function clearDeathDate() { $this->deathDate_ = null; }
    public function hasDeathDate() { return $this->deathDate_ !== null; }
    public function getDeathDate() { if($this->deathDate_ === null) return ""; else return $this->deathDate_; }
    public function setDeathDate($value) { $this->deathDate_ = $value; }
    
    // required string death_place = 15;

    private $deathPlace_ = null;
    public function clearDeathPlace() { $this->deathPlace_ = null; }
    public function hasDeathPlace() { return $this->deathPlace_ !== null; }
    public function getDeathPlace() { if($this->deathPlace_ === null) return ""; else return $this->deathPlace_; }
    public function setDeathPlace($value) { $this->deathPlace_ = $value; }
    
    // required .geneweb.api.object.DeathType death_type = 16;

    private $deathType_ = null;
    public function clearDeathType() { $this->deathType_ = null; }
    public function hasDeathType() { return $this->deathType_ !== null; }
    public function getDeathType() { if($this->deathType_ === null) return \geneweb\api\object\DeathType::NOT_DEAD; else return $this->deathType_; }
    public function setDeathType($value) { $this->deathType_ = $value; }
    
    // required string burial_date = 17;

    private $burialDate_ = null;
    public function clearBurialDate() { $this->burialDate_ = null; }
    public function hasBurialDate() { return $this->burialDate_ !== null; }
    public function getBurialDate() { if($this->burialDate_ === null) return ""; else return $this->burialDate_; }
    public function setBurialDate($value) { $this->burialDate_ = $value; }
    
    // required string burial_place = 18;

    private $burialPlace_ = null;
    public function clearBurialPlace() { $this->burialPlace_ = null; }
    public function hasBurialPlace() { return $this->burialPlace_ !== null; }
    public function getBurialPlace() { if($this->burialPlace_ === null) return ""; else return $this->burialPlace_; }
    public function setBurialPlace($value) { $this->burialPlace_ = $value; }
    
    // repeated .geneweb.api.object.Spouse spouses = 19;

    private $spouses_ = null;
    public function clearSpouses() { $this->spouses_ = null; }
    public function getSpousesCount() { if ($this->spouses_ === null ) return 0; else return count($this->spouses_); }
    public function getSpouses($index) { return $this->spouses_[$index]; }
    public function getSpousesArray() { if ($this->spouses_ === null ) return array(); else return $this->spouses_; }
    public function setSpouses($index, $value) {$this->spouses_[$index] = $value;	}
    public function addSpouses($value) { $this->spouses_[] = $value; }
    public function addAllSpouses(array $values) { foreach($values as $value) {$this->spouses_[] = $value;} }
    
    // required bool ascend = 20;

    private $ascend_ = null;
    public function clearAscend() { $this->ascend_ = null; }
    public function hasAscend() { return $this->ascend_ !== null; }
    public function getAscend() { if($this->ascend_ === null) return false; else return $this->ascend_; }
    public function setAscend($value) { $this->ascend_ = $value; }
    
    // required bool descend = 21;

    private $descend_ = null;
    public function clearDescend() { $this->descend_ = null; }
    public function hasDescend() { return $this->descend_ !== null; }
    public function getDescend() { if($this->descend_ === null) return false; else return $this->descend_; }
    public function setDescend($value) { $this->descend_ = $value; }
    
    // required bool visible_for_visitors = 22;

    private $visibleForVisitors_ = null;
    public function clearVisibleForVisitors() { $this->visibleForVisitors_ = null; }
    public function hasVisibleForVisitors() { return $this->visibleForVisitors_ !== null; }
    public function getVisibleForVisitors() { if($this->visibleForVisitors_ === null) return false; else return $this->visibleForVisitors_; }
    public function setVisibleForVisitors($value) { $this->visibleForVisitors_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.Person)
  }
  
  // message geneweb.api.object.FullPerson
  class FullPerson {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\FullPerson: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->sosa_ = $tmp;
            $limit-=$len;
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->n_ = $tmp;
            $limit-=$len;
            break;
          case 3:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->p_ = $tmp;
            $limit-=$len;
            break;
          case 4:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->oc_ = $tmp;
            
            break;
          case 5:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->index_ = $tmp;
            
            break;
          case 6:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->sex_ = $tmp;
            
            break;
          case 7:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->lastname_ = $tmp;
            $limit-=$len;
            break;
          case 8:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->firstname_ = $tmp;
            $limit-=$len;
            break;
          case 9:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->publicName_ = $tmp;
            $limit-=$len;
            break;
          case 10:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->aliases_[] = $tmp;
            $limit-=$len;
            break;
          case 11:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->qualifiers_[] = $tmp;
            $limit-=$len;
            break;
          case 12:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->firstnameAliases_[] = $tmp;
            $limit-=$len;
            break;
          case 13:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->surnameAliases_[] = $tmp;
            $limit-=$len;
            break;
          case 15:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->image_ = $tmp;
            $limit-=$len;
            break;
          case 16:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->birthDate_ = $tmp;
            $limit-=$len;
            break;
          case 17:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->birthPlace_ = $tmp;
            $limit-=$len;
            break;
          case 18:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->birthSrc_ = $tmp;
            $limit-=$len;
            break;
          case 19:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->baptismDate_ = $tmp;
            $limit-=$len;
            break;
          case 20:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->baptismPlace_ = $tmp;
            $limit-=$len;
            break;
          case 21:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->baptismSrc_ = $tmp;
            $limit-=$len;
            break;
          case 22:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->deathDate_ = $tmp;
            $limit-=$len;
            break;
          case 23:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->deathPlace_ = $tmp;
            $limit-=$len;
            break;
          case 24:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->deathSrc_ = $tmp;
            $limit-=$len;
            break;
          case 25:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->deathType_ = $tmp;
            
            break;
          case 26:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->burialDate_ = $tmp;
            $limit-=$len;
            break;
          case 27:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->burialPlace_ = $tmp;
            $limit-=$len;
            break;
          case 28:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->burialSrc_ = $tmp;
            $limit-=$len;
            break;
          case 30:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->occupation_ = $tmp;
            $limit-=$len;
            break;
          case 31:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->psources_ = $tmp;
            $limit-=$len;
            break;
          case 32:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->titles_[] = new \geneweb\api\object\Title($fp, $len);
            ASSERT('$len == 0');
            break;
          case 33:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->related_[] = new \geneweb\api\object\InternalInt32($fp, $len);
            ASSERT('$len == 0');
            break;
          case 34:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->rparents_[] = new \geneweb\api\object\RelationParent($fp, $len);
            ASSERT('$len == 0');
            break;
          case 35:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->visibleForVisitors_ = $tmp > 0 ? true : false;
            break;
          case 36:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->parents_ = $tmp;
            
            break;
          case 37:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->families_[] = new \geneweb\api\object\InternalInt32($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->sosa_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, strlen($this->sosa_));
        fwrite($fp, $this->sosa_);
      }
      if (!is_null($this->n_)) {
        fwrite($fp, "\x12");
        \Protobuf::write_varint($fp, strlen($this->n_));
        fwrite($fp, $this->n_);
      }
      if (!is_null($this->p_)) {
        fwrite($fp, "\x1a");
        \Protobuf::write_varint($fp, strlen($this->p_));
        fwrite($fp, $this->p_);
      }
      if (!is_null($this->oc_)) {
        fwrite($fp, " ");
        \Protobuf::write_varint($fp, $this->oc_);
      }
      if (!is_null($this->index_)) {
        fwrite($fp, "(");
        \Protobuf::write_varint($fp, $this->index_);
      }
      if (!is_null($this->sex_)) {
        fwrite($fp, "0");
        \Protobuf::write_varint($fp, $this->sex_);
      }
      if (!is_null($this->lastname_)) {
        fwrite($fp, ":");
        \Protobuf::write_varint($fp, strlen($this->lastname_));
        fwrite($fp, $this->lastname_);
      }
      if (!is_null($this->firstname_)) {
        fwrite($fp, "B");
        \Protobuf::write_varint($fp, strlen($this->firstname_));
        fwrite($fp, $this->firstname_);
      }
      if (!is_null($this->publicName_)) {
        fwrite($fp, "J");
        \Protobuf::write_varint($fp, strlen($this->publicName_));
        fwrite($fp, $this->publicName_);
      }
      if (!is_null($this->aliases_))
        foreach($this->aliases_ as $v) {
          fwrite($fp, "R");
          \Protobuf::write_varint($fp, strlen($v));
          fwrite($fp, $v);
        }
      if (!is_null($this->qualifiers_))
        foreach($this->qualifiers_ as $v) {
          fwrite($fp, "Z");
          \Protobuf::write_varint($fp, strlen($v));
          fwrite($fp, $v);
        }
      if (!is_null($this->firstnameAliases_))
        foreach($this->firstnameAliases_ as $v) {
          fwrite($fp, "b");
          \Protobuf::write_varint($fp, strlen($v));
          fwrite($fp, $v);
        }
      if (!is_null($this->surnameAliases_))
        foreach($this->surnameAliases_ as $v) {
          fwrite($fp, "j");
          \Protobuf::write_varint($fp, strlen($v));
          fwrite($fp, $v);
        }
      if (!is_null($this->image_)) {
        fwrite($fp, "z");
        \Protobuf::write_varint($fp, strlen($this->image_));
        fwrite($fp, $this->image_);
      }
      if (!is_null($this->birthDate_)) {
        fwrite($fp, "\x82\x01");
        \Protobuf::write_varint($fp, strlen($this->birthDate_));
        fwrite($fp, $this->birthDate_);
      }
      if (!is_null($this->birthPlace_)) {
        fwrite($fp, "\x8a\x01");
        \Protobuf::write_varint($fp, strlen($this->birthPlace_));
        fwrite($fp, $this->birthPlace_);
      }
      if (!is_null($this->birthSrc_)) {
        fwrite($fp, "\x92\x01");
        \Protobuf::write_varint($fp, strlen($this->birthSrc_));
        fwrite($fp, $this->birthSrc_);
      }
      if (!is_null($this->baptismDate_)) {
        fwrite($fp, "\x9a\x01");
        \Protobuf::write_varint($fp, strlen($this->baptismDate_));
        fwrite($fp, $this->baptismDate_);
      }
      if (!is_null($this->baptismPlace_)) {
        fwrite($fp, "\xa2\x01");
        \Protobuf::write_varint($fp, strlen($this->baptismPlace_));
        fwrite($fp, $this->baptismPlace_);
      }
      if (!is_null($this->baptismSrc_)) {
        fwrite($fp, "\xaa\x01");
        \Protobuf::write_varint($fp, strlen($this->baptismSrc_));
        fwrite($fp, $this->baptismSrc_);
      }
      if (!is_null($this->deathDate_)) {
        fwrite($fp, "\xb2\x01");
        \Protobuf::write_varint($fp, strlen($this->deathDate_));
        fwrite($fp, $this->deathDate_);
      }
      if (!is_null($this->deathPlace_)) {
        fwrite($fp, "\xba\x01");
        \Protobuf::write_varint($fp, strlen($this->deathPlace_));
        fwrite($fp, $this->deathPlace_);
      }
      if (!is_null($this->deathSrc_)) {
        fwrite($fp, "\xc2\x01");
        \Protobuf::write_varint($fp, strlen($this->deathSrc_));
        fwrite($fp, $this->deathSrc_);
      }
      if (!is_null($this->deathType_)) {
        fwrite($fp, "\xc8\x01");
        \Protobuf::write_varint($fp, $this->deathType_);
      }
      if (!is_null($this->burialDate_)) {
        fwrite($fp, "\xd2\x01");
        \Protobuf::write_varint($fp, strlen($this->burialDate_));
        fwrite($fp, $this->burialDate_);
      }
      if (!is_null($this->burialPlace_)) {
        fwrite($fp, "\xda\x01");
        \Protobuf::write_varint($fp, strlen($this->burialPlace_));
        fwrite($fp, $this->burialPlace_);
      }
      if (!is_null($this->burialSrc_)) {
        fwrite($fp, "\xe2\x01");
        \Protobuf::write_varint($fp, strlen($this->burialSrc_));
        fwrite($fp, $this->burialSrc_);
      }
      if (!is_null($this->occupation_)) {
        fwrite($fp, "\xf2\x01");
        \Protobuf::write_varint($fp, strlen($this->occupation_));
        fwrite($fp, $this->occupation_);
      }
      if (!is_null($this->psources_)) {
        fwrite($fp, "\xfa\x01");
        \Protobuf::write_varint($fp, strlen($this->psources_));
        fwrite($fp, $this->psources_);
      }
      if (!is_null($this->titles_))
        foreach($this->titles_ as $v) {
          fwrite($fp, "\x82\x02");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->related_))
        foreach($this->related_ as $v) {
          fwrite($fp, "\x8a\x02");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->rparents_))
        foreach($this->rparents_ as $v) {
          fwrite($fp, "\x92\x02");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->visibleForVisitors_)) {
        fwrite($fp, "\x98\x02");
        \Protobuf::write_varint($fp, $this->visibleForVisitors_ ? 1 : 0);
      }
      if (!is_null($this->parents_)) {
        fwrite($fp, "\xa0\x02");
        \Protobuf::write_varint($fp, $this->parents_);
      }
      if (!is_null($this->families_))
        foreach($this->families_ as $v) {
          fwrite($fp, "\xaa\x02");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->sosa_)) {
        $l = strlen($this->sosa_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->n_)) {
        $l = strlen($this->n_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->p_)) {
        $l = strlen($this->p_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->oc_)) {
        $size += 1 + \Protobuf::size_varint($this->oc_);
      }
      if (!is_null($this->index_)) {
        $size += 1 + \Protobuf::size_varint($this->index_);
      }
      if (!is_null($this->sex_)) {
        $size += 1 + \Protobuf::size_varint($this->sex_);
      }
      if (!is_null($this->lastname_)) {
        $l = strlen($this->lastname_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->firstname_)) {
        $l = strlen($this->firstname_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->publicName_)) {
        $l = strlen($this->publicName_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->aliases_))
        foreach($this->aliases_ as $v) {
          $l = strlen($v);
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->qualifiers_))
        foreach($this->qualifiers_ as $v) {
          $l = strlen($v);
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->firstnameAliases_))
        foreach($this->firstnameAliases_ as $v) {
          $l = strlen($v);
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->surnameAliases_))
        foreach($this->surnameAliases_ as $v) {
          $l = strlen($v);
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->image_)) {
        $l = strlen($this->image_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->birthDate_)) {
        $l = strlen($this->birthDate_);
        $size += 2 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->birthPlace_)) {
        $l = strlen($this->birthPlace_);
        $size += 2 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->birthSrc_)) {
        $l = strlen($this->birthSrc_);
        $size += 2 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->baptismDate_)) {
        $l = strlen($this->baptismDate_);
        $size += 2 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->baptismPlace_)) {
        $l = strlen($this->baptismPlace_);
        $size += 2 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->baptismSrc_)) {
        $l = strlen($this->baptismSrc_);
        $size += 2 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->deathDate_)) {
        $l = strlen($this->deathDate_);
        $size += 2 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->deathPlace_)) {
        $l = strlen($this->deathPlace_);
        $size += 2 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->deathSrc_)) {
        $l = strlen($this->deathSrc_);
        $size += 2 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->deathType_)) {
        $size += 2 + \Protobuf::size_varint($this->deathType_);
      }
      if (!is_null($this->burialDate_)) {
        $l = strlen($this->burialDate_);
        $size += 2 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->burialPlace_)) {
        $l = strlen($this->burialPlace_);
        $size += 2 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->burialSrc_)) {
        $l = strlen($this->burialSrc_);
        $size += 2 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->occupation_)) {
        $l = strlen($this->occupation_);
        $size += 2 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->psources_)) {
        $l = strlen($this->psources_);
        $size += 2 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->titles_))
        foreach($this->titles_ as $v) {
          $l = $v->size();
          $size += 2 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->related_))
        foreach($this->related_ as $v) {
          $l = $v->size();
          $size += 2 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->rparents_))
        foreach($this->rparents_ as $v) {
          $l = $v->size();
          $size += 2 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->visibleForVisitors_)) {
        $size += 3;
      }
      if (!is_null($this->parents_)) {
        $size += 2 + \Protobuf::size_varint($this->parents_);
      }
      if (!is_null($this->families_))
        foreach($this->families_ as $v) {
          $l = $v->size();
          $size += 2 + \Protobuf::size_varint($l) + $l;
        }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->sosa_ === null) return false;
      if ($this->n_ === null) return false;
      if ($this->p_ === null) return false;
      if ($this->oc_ === null) return false;
      if ($this->index_ === null) return false;
      if ($this->sex_ === null) return false;
      if ($this->lastname_ === null) return false;
      if ($this->firstname_ === null) return false;
      if ($this->deathType_ === null) return false;
      if ($this->visibleForVisitors_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('sosa_', $this->sosa_)
           . \Protobuf::toString('n_', $this->n_)
           . \Protobuf::toString('p_', $this->p_)
           . \Protobuf::toString('oc_', $this->oc_)
           . \Protobuf::toString('index_', $this->index_)
           . \Protobuf::toString('sex_', \geneweb\api\object\Sex::toString($this->sex_))
           . \Protobuf::toString('lastname_', $this->lastname_)
           . \Protobuf::toString('firstname_', $this->firstname_)
           . \Protobuf::toString('publicName_', $this->publicName_)
           . \Protobuf::toString('aliases_', $this->aliases_)
           . \Protobuf::toString('qualifiers_', $this->qualifiers_)
           . \Protobuf::toString('firstnameAliases_', $this->firstnameAliases_)
           . \Protobuf::toString('surnameAliases_', $this->surnameAliases_)
           . \Protobuf::toString('image_', $this->image_)
           . \Protobuf::toString('birthDate_', $this->birthDate_)
           . \Protobuf::toString('birthPlace_', $this->birthPlace_)
           . \Protobuf::toString('birthSrc_', $this->birthSrc_)
           . \Protobuf::toString('baptismDate_', $this->baptismDate_)
           . \Protobuf::toString('baptismPlace_', $this->baptismPlace_)
           . \Protobuf::toString('baptismSrc_', $this->baptismSrc_)
           . \Protobuf::toString('deathDate_', $this->deathDate_)
           . \Protobuf::toString('deathPlace_', $this->deathPlace_)
           . \Protobuf::toString('deathSrc_', $this->deathSrc_)
           . \Protobuf::toString('deathType_', \geneweb\api\object\DeathType::toString($this->deathType_))
           . \Protobuf::toString('burialDate_', $this->burialDate_)
           . \Protobuf::toString('burialPlace_', $this->burialPlace_)
           . \Protobuf::toString('burialSrc_', $this->burialSrc_)
           . \Protobuf::toString('occupation_', $this->occupation_)
           . \Protobuf::toString('psources_', $this->psources_)
           . \Protobuf::toString('titles_', $this->titles_)
           . \Protobuf::toString('related_', $this->related_)
           . \Protobuf::toString('rparents_', $this->rparents_)
           . \Protobuf::toString('visibleForVisitors_', $this->visibleForVisitors_)
           . \Protobuf::toString('parents_', $this->parents_)
           . \Protobuf::toString('families_', $this->families_);
    }
    
    // required string sosa = 1;

    private $sosa_ = null;
    public function clearSosa() { $this->sosa_ = null; }
    public function hasSosa() { return $this->sosa_ !== null; }
    public function getSosa() { if($this->sosa_ === null) return ""; else return $this->sosa_; }
    public function setSosa($value) { $this->sosa_ = $value; }
    
    // required string n = 2;

    private $n_ = null;
    public function clearN() { $this->n_ = null; }
    public function hasN() { return $this->n_ !== null; }
    public function getN() { if($this->n_ === null) return ""; else return $this->n_; }
    public function setN($value) { $this->n_ = $value; }
    
    // required string p = 3;

    private $p_ = null;
    public function clearP() { $this->p_ = null; }
    public function hasP() { return $this->p_ !== null; }
    public function getP() { if($this->p_ === null) return ""; else return $this->p_; }
    public function setP($value) { $this->p_ = $value; }
    
    // required int32 oc = 4;

    private $oc_ = null;
    public function clearOc() { $this->oc_ = null; }
    public function hasOc() { return $this->oc_ !== null; }
    public function getOc() { if($this->oc_ === null) return 0; else return $this->oc_; }
    public function setOc($value) { $this->oc_ = $value; }
    
    // required int32 index = 5;

    private $index_ = null;
    public function clearIndex() { $this->index_ = null; }
    public function hasIndex() { return $this->index_ !== null; }
    public function getIndex() { if($this->index_ === null) return 0; else return $this->index_; }
    public function setIndex($value) { $this->index_ = $value; }
    
    // required .geneweb.api.object.Sex sex = 6;

    private $sex_ = null;
    public function clearSex() { $this->sex_ = null; }
    public function hasSex() { return $this->sex_ !== null; }
    public function getSex() { if($this->sex_ === null) return \geneweb\api\object\Sex::MALE; else return $this->sex_; }
    public function setSex($value) { $this->sex_ = $value; }
    
    // required string lastname = 7;

    private $lastname_ = null;
    public function clearLastname() { $this->lastname_ = null; }
    public function hasLastname() { return $this->lastname_ !== null; }
    public function getLastname() { if($this->lastname_ === null) return ""; else return $this->lastname_; }
    public function setLastname($value) { $this->lastname_ = $value; }
    
    // required string firstname = 8;

    private $firstname_ = null;
    public function clearFirstname() { $this->firstname_ = null; }
    public function hasFirstname() { return $this->firstname_ !== null; }
    public function getFirstname() { if($this->firstname_ === null) return ""; else return $this->firstname_; }
    public function setFirstname($value) { $this->firstname_ = $value; }
    
    // optional string public_name = 9;

    private $publicName_ = null;
    public function clearPublicName() { $this->publicName_ = null; }
    public function hasPublicName() { return $this->publicName_ !== null; }
    public function getPublicName() { if($this->publicName_ === null) return ""; else return $this->publicName_; }
    public function setPublicName($value) { $this->publicName_ = $value; }
    
    // repeated string aliases = 10;

    private $aliases_ = null;
    public function clearAliases() { $this->aliases_ = null; }
    public function getAliasesCount() { if ($this->aliases_ === null ) return 0; else return count($this->aliases_); }
    public function getAliases($index) { return $this->aliases_[$index]; }
    public function getAliasesArray() { if ($this->aliases_ === null ) return array(); else return $this->aliases_; }
    public function setAliases($index, $value) {$this->aliases_[$index] = $value;	}
    public function addAliases($value) { $this->aliases_[] = $value; }
    public function addAllAliases(array $values) { foreach($values as $value) {$this->aliases_[] = $value;} }
    
    // repeated string qualifiers = 11;

    private $qualifiers_ = null;
    public function clearQualifiers() { $this->qualifiers_ = null; }
    public function getQualifiersCount() { if ($this->qualifiers_ === null ) return 0; else return count($this->qualifiers_); }
    public function getQualifiers($index) { return $this->qualifiers_[$index]; }
    public function getQualifiersArray() { if ($this->qualifiers_ === null ) return array(); else return $this->qualifiers_; }
    public function setQualifiers($index, $value) {$this->qualifiers_[$index] = $value;	}
    public function addQualifiers($value) { $this->qualifiers_[] = $value; }
    public function addAllQualifiers(array $values) { foreach($values as $value) {$this->qualifiers_[] = $value;} }
    
    // repeated string firstname_aliases = 12;

    private $firstnameAliases_ = null;
    public function clearFirstnameAliases() { $this->firstnameAliases_ = null; }
    public function getFirstnameAliasesCount() { if ($this->firstnameAliases_ === null ) return 0; else return count($this->firstnameAliases_); }
    public function getFirstnameAliases($index) { return $this->firstnameAliases_[$index]; }
    public function getFirstnameAliasesArray() { if ($this->firstnameAliases_ === null ) return array(); else return $this->firstnameAliases_; }
    public function setFirstnameAliases($index, $value) {$this->firstnameAliases_[$index] = $value;	}
    public function addFirstnameAliases($value) { $this->firstnameAliases_[] = $value; }
    public function addAllFirstnameAliases(array $values) { foreach($values as $value) {$this->firstnameAliases_[] = $value;} }
    
    // repeated string surname_aliases = 13;

    private $surnameAliases_ = null;
    public function clearSurnameAliases() { $this->surnameAliases_ = null; }
    public function getSurnameAliasesCount() { if ($this->surnameAliases_ === null ) return 0; else return count($this->surnameAliases_); }
    public function getSurnameAliases($index) { return $this->surnameAliases_[$index]; }
    public function getSurnameAliasesArray() { if ($this->surnameAliases_ === null ) return array(); else return $this->surnameAliases_; }
    public function setSurnameAliases($index, $value) {$this->surnameAliases_[$index] = $value;	}
    public function addSurnameAliases($value) { $this->surnameAliases_[] = $value; }
    public function addAllSurnameAliases(array $values) { foreach($values as $value) {$this->surnameAliases_[] = $value;} }
    
    // optional string image = 15;

    private $image_ = null;
    public function clearImage() { $this->image_ = null; }
    public function hasImage() { return $this->image_ !== null; }
    public function getImage() { if($this->image_ === null) return ""; else return $this->image_; }
    public function setImage($value) { $this->image_ = $value; }
    
    // optional string birth_date = 16;

    private $birthDate_ = null;
    public function clearBirthDate() { $this->birthDate_ = null; }
    public function hasBirthDate() { return $this->birthDate_ !== null; }
    public function getBirthDate() { if($this->birthDate_ === null) return ""; else return $this->birthDate_; }
    public function setBirthDate($value) { $this->birthDate_ = $value; }
    
    // optional string birth_place = 17;

    private $birthPlace_ = null;
    public function clearBirthPlace() { $this->birthPlace_ = null; }
    public function hasBirthPlace() { return $this->birthPlace_ !== null; }
    public function getBirthPlace() { if($this->birthPlace_ === null) return ""; else return $this->birthPlace_; }
    public function setBirthPlace($value) { $this->birthPlace_ = $value; }
    
    // optional string birth_src = 18;

    private $birthSrc_ = null;
    public function clearBirthSrc() { $this->birthSrc_ = null; }
    public function hasBirthSrc() { return $this->birthSrc_ !== null; }
    public function getBirthSrc() { if($this->birthSrc_ === null) return ""; else return $this->birthSrc_; }
    public function setBirthSrc($value) { $this->birthSrc_ = $value; }
    
    // optional string baptism_date = 19;

    private $baptismDate_ = null;
    public function clearBaptismDate() { $this->baptismDate_ = null; }
    public function hasBaptismDate() { return $this->baptismDate_ !== null; }
    public function getBaptismDate() { if($this->baptismDate_ === null) return ""; else return $this->baptismDate_; }
    public function setBaptismDate($value) { $this->baptismDate_ = $value; }
    
    // optional string baptism_place = 20;

    private $baptismPlace_ = null;
    public function clearBaptismPlace() { $this->baptismPlace_ = null; }
    public function hasBaptismPlace() { return $this->baptismPlace_ !== null; }
    public function getBaptismPlace() { if($this->baptismPlace_ === null) return ""; else return $this->baptismPlace_; }
    public function setBaptismPlace($value) { $this->baptismPlace_ = $value; }
    
    // optional string baptism_src = 21;

    private $baptismSrc_ = null;
    public function clearBaptismSrc() { $this->baptismSrc_ = null; }
    public function hasBaptismSrc() { return $this->baptismSrc_ !== null; }
    public function getBaptismSrc() { if($this->baptismSrc_ === null) return ""; else return $this->baptismSrc_; }
    public function setBaptismSrc($value) { $this->baptismSrc_ = $value; }
    
    // optional string death_date = 22;

    private $deathDate_ = null;
    public function clearDeathDate() { $this->deathDate_ = null; }
    public function hasDeathDate() { return $this->deathDate_ !== null; }
    public function getDeathDate() { if($this->deathDate_ === null) return ""; else return $this->deathDate_; }
    public function setDeathDate($value) { $this->deathDate_ = $value; }
    
    // optional string death_place = 23;

    private $deathPlace_ = null;
    public function clearDeathPlace() { $this->deathPlace_ = null; }
    public function hasDeathPlace() { return $this->deathPlace_ !== null; }
    public function getDeathPlace() { if($this->deathPlace_ === null) return ""; else return $this->deathPlace_; }
    public function setDeathPlace($value) { $this->deathPlace_ = $value; }
    
    // optional string death_src = 24;

    private $deathSrc_ = null;
    public function clearDeathSrc() { $this->deathSrc_ = null; }
    public function hasDeathSrc() { return $this->deathSrc_ !== null; }
    public function getDeathSrc() { if($this->deathSrc_ === null) return ""; else return $this->deathSrc_; }
    public function setDeathSrc($value) { $this->deathSrc_ = $value; }
    
    // required .geneweb.api.object.DeathType death_type = 25;

    private $deathType_ = null;
    public function clearDeathType() { $this->deathType_ = null; }
    public function hasDeathType() { return $this->deathType_ !== null; }
    public function getDeathType() { if($this->deathType_ === null) return \geneweb\api\object\DeathType::NOT_DEAD; else return $this->deathType_; }
    public function setDeathType($value) { $this->deathType_ = $value; }
    
    // optional string burial_date = 26;

    private $burialDate_ = null;
    public function clearBurialDate() { $this->burialDate_ = null; }
    public function hasBurialDate() { return $this->burialDate_ !== null; }
    public function getBurialDate() { if($this->burialDate_ === null) return ""; else return $this->burialDate_; }
    public function setBurialDate($value) { $this->burialDate_ = $value; }
    
    // optional string burial_place = 27;

    private $burialPlace_ = null;
    public function clearBurialPlace() { $this->burialPlace_ = null; }
    public function hasBurialPlace() { return $this->burialPlace_ !== null; }
    public function getBurialPlace() { if($this->burialPlace_ === null) return ""; else return $this->burialPlace_; }
    public function setBurialPlace($value) { $this->burialPlace_ = $value; }
    
    // optional string burial_src = 28;

    private $burialSrc_ = null;
    public function clearBurialSrc() { $this->burialSrc_ = null; }
    public function hasBurialSrc() { return $this->burialSrc_ !== null; }
    public function getBurialSrc() { if($this->burialSrc_ === null) return ""; else return $this->burialSrc_; }
    public function setBurialSrc($value) { $this->burialSrc_ = $value; }
    
    // optional string occupation = 30;

    private $occupation_ = null;
    public function clearOccupation() { $this->occupation_ = null; }
    public function hasOccupation() { return $this->occupation_ !== null; }
    public function getOccupation() { if($this->occupation_ === null) return ""; else return $this->occupation_; }
    public function setOccupation($value) { $this->occupation_ = $value; }
    
    // optional string psources = 31;

    private $psources_ = null;
    public function clearPsources() { $this->psources_ = null; }
    public function hasPsources() { return $this->psources_ !== null; }
    public function getPsources() { if($this->psources_ === null) return ""; else return $this->psources_; }
    public function setPsources($value) { $this->psources_ = $value; }
    
    // repeated .geneweb.api.object.Title titles = 32;

    private $titles_ = null;
    public function clearTitles() { $this->titles_ = null; }
    public function getTitlesCount() { if ($this->titles_ === null ) return 0; else return count($this->titles_); }
    public function getTitles($index) { return $this->titles_[$index]; }
    public function getTitlesArray() { if ($this->titles_ === null ) return array(); else return $this->titles_; }
    public function setTitles($index, $value) {$this->titles_[$index] = $value;	}
    public function addTitles($value) { $this->titles_[] = $value; }
    public function addAllTitles(array $values) { foreach($values as $value) {$this->titles_[] = $value;} }
    
    // repeated .geneweb.api.object.InternalInt32 related = 33;

    private $related_ = null;
    public function clearRelated() { $this->related_ = null; }
    public function getRelatedCount() { if ($this->related_ === null ) return 0; else return count($this->related_); }
    public function getRelated($index) { return $this->related_[$index]; }
    public function getRelatedArray() { if ($this->related_ === null ) return array(); else return $this->related_; }
    public function setRelated($index, $value) {$this->related_[$index] = $value;	}
    public function addRelated($value) { $this->related_[] = $value; }
    public function addAllRelated(array $values) { foreach($values as $value) {$this->related_[] = $value;} }
    
    // repeated .geneweb.api.object.RelationParent rparents = 34;

    private $rparents_ = null;
    public function clearRparents() { $this->rparents_ = null; }
    public function getRparentsCount() { if ($this->rparents_ === null ) return 0; else return count($this->rparents_); }
    public function getRparents($index) { return $this->rparents_[$index]; }
    public function getRparentsArray() { if ($this->rparents_ === null ) return array(); else return $this->rparents_; }
    public function setRparents($index, $value) {$this->rparents_[$index] = $value;	}
    public function addRparents($value) { $this->rparents_[] = $value; }
    public function addAllRparents(array $values) { foreach($values as $value) {$this->rparents_[] = $value;} }
    
    // required bool visible_for_visitors = 35;

    private $visibleForVisitors_ = null;
    public function clearVisibleForVisitors() { $this->visibleForVisitors_ = null; }
    public function hasVisibleForVisitors() { return $this->visibleForVisitors_ !== null; }
    public function getVisibleForVisitors() { if($this->visibleForVisitors_ === null) return false; else return $this->visibleForVisitors_; }
    public function setVisibleForVisitors($value) { $this->visibleForVisitors_ = $value; }
    
    // optional int32 parents = 36;

    private $parents_ = null;
    public function clearParents() { $this->parents_ = null; }
    public function hasParents() { return $this->parents_ !== null; }
    public function getParents() { if($this->parents_ === null) return 0; else return $this->parents_; }
    public function setParents($value) { $this->parents_ = $value; }
    
    // repeated .geneweb.api.object.InternalInt32 families = 37;

    private $families_ = null;
    public function clearFamilies() { $this->families_ = null; }
    public function getFamiliesCount() { if ($this->families_ === null ) return 0; else return count($this->families_); }
    public function getFamilies($index) { return $this->families_[$index]; }
    public function getFamiliesArray() { if ($this->families_ === null ) return array(); else return $this->families_; }
    public function setFamilies($index, $value) {$this->families_[$index] = $value;	}
    public function addFamilies($value) { $this->families_[] = $value; }
    public function addAllFamilies(array $values) { foreach($values as $value) {$this->families_[] = $value;} }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.FullPerson)
  }
  
  // message geneweb.api.object.FullFamily
  class FullFamily {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\FullFamily: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->fsources_ = $tmp;
            $limit-=$len;
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->marriageDate_ = $tmp;
            $limit-=$len;
            break;
          case 3:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->marriagePlace_ = $tmp;
            $limit-=$len;
            break;
          case 4:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->marriageSrc_ = $tmp;
            $limit-=$len;
            break;
          case 5:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->marriageType_ = $tmp;
            
            break;
          case 6:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->divorceType_ = $tmp;
            
            break;
          case 7:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->divorceDate_ = $tmp;
            $limit-=$len;
            break;
          case 8:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->witnesses_[] = new \geneweb\api\object\InternalInt32($fp, $len);
            ASSERT('$len == 0');
            break;
          case 9:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->father_ = $tmp;
            
            break;
          case 10:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->mother_ = $tmp;
            
            break;
          case 11:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->children_[] = new \geneweb\api\object\InternalInt32($fp, $len);
            ASSERT('$len == 0');
            break;
          case 12:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->index_ = $tmp;
            
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->fsources_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, strlen($this->fsources_));
        fwrite($fp, $this->fsources_);
      }
      if (!is_null($this->marriageDate_)) {
        fwrite($fp, "\x12");
        \Protobuf::write_varint($fp, strlen($this->marriageDate_));
        fwrite($fp, $this->marriageDate_);
      }
      if (!is_null($this->marriagePlace_)) {
        fwrite($fp, "\x1a");
        \Protobuf::write_varint($fp, strlen($this->marriagePlace_));
        fwrite($fp, $this->marriagePlace_);
      }
      if (!is_null($this->marriageSrc_)) {
        fwrite($fp, "\"");
        \Protobuf::write_varint($fp, strlen($this->marriageSrc_));
        fwrite($fp, $this->marriageSrc_);
      }
      if (!is_null($this->marriageType_)) {
        fwrite($fp, "(");
        \Protobuf::write_varint($fp, $this->marriageType_);
      }
      if (!is_null($this->divorceType_)) {
        fwrite($fp, "0");
        \Protobuf::write_varint($fp, $this->divorceType_);
      }
      if (!is_null($this->divorceDate_)) {
        fwrite($fp, ":");
        \Protobuf::write_varint($fp, strlen($this->divorceDate_));
        fwrite($fp, $this->divorceDate_);
      }
      if (!is_null($this->witnesses_))
        foreach($this->witnesses_ as $v) {
          fwrite($fp, "B");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->father_)) {
        fwrite($fp, "H");
        \Protobuf::write_varint($fp, $this->father_);
      }
      if (!is_null($this->mother_)) {
        fwrite($fp, "P");
        \Protobuf::write_varint($fp, $this->mother_);
      }
      if (!is_null($this->children_))
        foreach($this->children_ as $v) {
          fwrite($fp, "Z");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->index_)) {
        fwrite($fp, "`");
        \Protobuf::write_varint($fp, $this->index_);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->fsources_)) {
        $l = strlen($this->fsources_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->marriageDate_)) {
        $l = strlen($this->marriageDate_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->marriagePlace_)) {
        $l = strlen($this->marriagePlace_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->marriageSrc_)) {
        $l = strlen($this->marriageSrc_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->marriageType_)) {
        $size += 1 + \Protobuf::size_varint($this->marriageType_);
      }
      if (!is_null($this->divorceType_)) {
        $size += 1 + \Protobuf::size_varint($this->divorceType_);
      }
      if (!is_null($this->divorceDate_)) {
        $l = strlen($this->divorceDate_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->witnesses_))
        foreach($this->witnesses_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->father_)) {
        $size += 1 + \Protobuf::size_varint($this->father_);
      }
      if (!is_null($this->mother_)) {
        $size += 1 + \Protobuf::size_varint($this->mother_);
      }
      if (!is_null($this->children_))
        foreach($this->children_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->index_)) {
        $size += 1 + \Protobuf::size_varint($this->index_);
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->marriageType_ === null) return false;
      if ($this->divorceType_ === null) return false;
      if ($this->father_ === null) return false;
      if ($this->mother_ === null) return false;
      if ($this->index_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('fsources_', $this->fsources_)
           . \Protobuf::toString('marriageDate_', $this->marriageDate_)
           . \Protobuf::toString('marriagePlace_', $this->marriagePlace_)
           . \Protobuf::toString('marriageSrc_', $this->marriageSrc_)
           . \Protobuf::toString('marriageType_', \geneweb\api\object\MarriageType::toString($this->marriageType_))
           . \Protobuf::toString('divorceType_', \geneweb\api\object\DivorceType::toString($this->divorceType_))
           . \Protobuf::toString('divorceDate_', $this->divorceDate_)
           . \Protobuf::toString('witnesses_', $this->witnesses_)
           . \Protobuf::toString('father_', $this->father_)
           . \Protobuf::toString('mother_', $this->mother_)
           . \Protobuf::toString('children_', $this->children_)
           . \Protobuf::toString('index_', $this->index_);
    }
    
    // optional string fsources = 1;

    private $fsources_ = null;
    public function clearFsources() { $this->fsources_ = null; }
    public function hasFsources() { return $this->fsources_ !== null; }
    public function getFsources() { if($this->fsources_ === null) return ""; else return $this->fsources_; }
    public function setFsources($value) { $this->fsources_ = $value; }
    
    // optional string marriage_date = 2;

    private $marriageDate_ = null;
    public function clearMarriageDate() { $this->marriageDate_ = null; }
    public function hasMarriageDate() { return $this->marriageDate_ !== null; }
    public function getMarriageDate() { if($this->marriageDate_ === null) return ""; else return $this->marriageDate_; }
    public function setMarriageDate($value) { $this->marriageDate_ = $value; }
    
    // optional string marriage_place = 3;

    private $marriagePlace_ = null;
    public function clearMarriagePlace() { $this->marriagePlace_ = null; }
    public function hasMarriagePlace() { return $this->marriagePlace_ !== null; }
    public function getMarriagePlace() { if($this->marriagePlace_ === null) return ""; else return $this->marriagePlace_; }
    public function setMarriagePlace($value) { $this->marriagePlace_ = $value; }
    
    // optional string marriage_src = 4;

    private $marriageSrc_ = null;
    public function clearMarriageSrc() { $this->marriageSrc_ = null; }
    public function hasMarriageSrc() { return $this->marriageSrc_ !== null; }
    public function getMarriageSrc() { if($this->marriageSrc_ === null) return ""; else return $this->marriageSrc_; }
    public function setMarriageSrc($value) { $this->marriageSrc_ = $value; }
    
    // required .geneweb.api.object.MarriageType marriage_type = 5;

    private $marriageType_ = null;
    public function clearMarriageType() { $this->marriageType_ = null; }
    public function hasMarriageType() { return $this->marriageType_ !== null; }
    public function getMarriageType() { if($this->marriageType_ === null) return \geneweb\api\object\MarriageType::MARRIED; else return $this->marriageType_; }
    public function setMarriageType($value) { $this->marriageType_ = $value; }
    
    // required .geneweb.api.object.DivorceType divorce_type = 6;

    private $divorceType_ = null;
    public function clearDivorceType() { $this->divorceType_ = null; }
    public function hasDivorceType() { return $this->divorceType_ !== null; }
    public function getDivorceType() { if($this->divorceType_ === null) return \geneweb\api\object\DivorceType::NOT_DIVORCED; else return $this->divorceType_; }
    public function setDivorceType($value) { $this->divorceType_ = $value; }
    
    // optional string divorce_date = 7;

    private $divorceDate_ = null;
    public function clearDivorceDate() { $this->divorceDate_ = null; }
    public function hasDivorceDate() { return $this->divorceDate_ !== null; }
    public function getDivorceDate() { if($this->divorceDate_ === null) return ""; else return $this->divorceDate_; }
    public function setDivorceDate($value) { $this->divorceDate_ = $value; }
    
    // repeated .geneweb.api.object.InternalInt32 witnesses = 8;

    private $witnesses_ = null;
    public function clearWitnesses() { $this->witnesses_ = null; }
    public function getWitnessesCount() { if ($this->witnesses_ === null ) return 0; else return count($this->witnesses_); }
    public function getWitnesses($index) { return $this->witnesses_[$index]; }
    public function getWitnessesArray() { if ($this->witnesses_ === null ) return array(); else return $this->witnesses_; }
    public function setWitnesses($index, $value) {$this->witnesses_[$index] = $value;	}
    public function addWitnesses($value) { $this->witnesses_[] = $value; }
    public function addAllWitnesses(array $values) { foreach($values as $value) {$this->witnesses_[] = $value;} }
    
    // required int32 father = 9;

    private $father_ = null;
    public function clearFather() { $this->father_ = null; }
    public function hasFather() { return $this->father_ !== null; }
    public function getFather() { if($this->father_ === null) return 0; else return $this->father_; }
    public function setFather($value) { $this->father_ = $value; }
    
    // required int32 mother = 10;

    private $mother_ = null;
    public function clearMother() { $this->mother_ = null; }
    public function hasMother() { return $this->mother_ !== null; }
    public function getMother() { if($this->mother_ === null) return 0; else return $this->mother_; }
    public function setMother($value) { $this->mother_ = $value; }
    
    // repeated .geneweb.api.object.InternalInt32 children = 11;

    private $children_ = null;
    public function clearChildren() { $this->children_ = null; }
    public function getChildrenCount() { if ($this->children_ === null ) return 0; else return count($this->children_); }
    public function getChildren($index) { return $this->children_[$index]; }
    public function getChildrenArray() { if ($this->children_ === null ) return array(); else return $this->children_; }
    public function setChildren($index, $value) {$this->children_[$index] = $value;	}
    public function addChildren($value) { $this->children_[] = $value; }
    public function addAllChildren(array $values) { foreach($values as $value) {$this->children_[] = $value;} }
    
    // required int32 index = 12;

    private $index_ = null;
    public function clearIndex() { $this->index_ = null; }
    public function hasIndex() { return $this->index_ !== null; }
    public function getIndex() { if($this->index_ === null) return 0; else return $this->index_; }
    public function setIndex($value) { $this->index_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.FullFamily)
  }
  
  // message geneweb.api.object.InternalInt32
  class InternalInt32 {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\InternalInt32: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->value_ = $tmp;
            
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->value_)) {
        fwrite($fp, "\x08");
        \Protobuf::write_varint($fp, $this->value_);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->value_)) {
        $size += 1 + \Protobuf::size_varint($this->value_);
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->value_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('value_', $this->value_);
    }
    
    // required int32 value = 1;

    private $value_ = null;
    public function clearValue() { $this->value_ = null; }
    public function hasValue() { return $this->value_ !== null; }
    public function getValue() { if($this->value_ === null) return 0; else return $this->value_; }
    public function setValue($value) { $this->value_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.InternalInt32)
  }
  
  // message geneweb.api.object.ListPersons
  class ListPersons {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\ListPersons: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->listPersons_[] = new \geneweb\api\object\Person($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->listPersons_))
        foreach($this->listPersons_ as $v) {
          fwrite($fp, "\x0a");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->listPersons_))
        foreach($this->listPersons_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      return $size;
    }
    
    public function validateRequired() {
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('listPersons_', $this->listPersons_);
    }
    
    // repeated .geneweb.api.object.Person list_persons = 1;

    private $listPersons_ = null;
    public function clearListPersons() { $this->listPersons_ = null; }
    public function getListPersonsCount() { if ($this->listPersons_ === null ) return 0; else return count($this->listPersons_); }
    public function getListPersons($index) { return $this->listPersons_[$index]; }
    public function getListPersonsArray() { if ($this->listPersons_ === null ) return array(); else return $this->listPersons_; }
    public function setListPersons($index, $value) {$this->listPersons_[$index] = $value;	}
    public function addListPersons($value) { $this->listPersons_[] = $value; }
    public function addAllListPersons(array $values) { foreach($values as $value) {$this->listPersons_[] = $value;} }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.ListPersons)
  }
  
  // message geneweb.api.object.ListFullPersons
  class ListFullPersons {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\ListFullPersons: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->persons_[] = new \geneweb\api\object\FullPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->persons_))
        foreach($this->persons_ as $v) {
          fwrite($fp, "\x0a");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->persons_))
        foreach($this->persons_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      return $size;
    }
    
    public function validateRequired() {
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('persons_', $this->persons_);
    }
    
    // repeated .geneweb.api.object.FullPerson persons = 1;

    private $persons_ = null;
    public function clearPersons() { $this->persons_ = null; }
    public function getPersonsCount() { if ($this->persons_ === null ) return 0; else return count($this->persons_); }
    public function getPersons($index) { return $this->persons_[$index]; }
    public function getPersonsArray() { if ($this->persons_ === null ) return array(); else return $this->persons_; }
    public function setPersons($index, $value) {$this->persons_[$index] = $value;	}
    public function addPersons($value) { $this->persons_[] = $value; }
    public function addAllPersons(array $values) { foreach($values as $value) {$this->persons_[] = $value;} }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.ListFullPersons)
  }
  
  // message geneweb.api.object.ListFullFamilies
  class ListFullFamilies {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\ListFullFamilies: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->families_[] = new \geneweb\api\object\FullFamily($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->families_))
        foreach($this->families_ as $v) {
          fwrite($fp, "\x0a");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->families_))
        foreach($this->families_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      return $size;
    }
    
    public function validateRequired() {
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('families_', $this->families_);
    }
    
    // repeated .geneweb.api.object.FullFamily families = 1;

    private $families_ = null;
    public function clearFamilies() { $this->families_ = null; }
    public function getFamiliesCount() { if ($this->families_ === null ) return 0; else return count($this->families_); }
    public function getFamilies($index) { return $this->families_[$index]; }
    public function getFamiliesArray() { if ($this->families_ === null ) return array(); else return $this->families_; }
    public function setFamilies($index, $value) {$this->families_[$index] = $value;	}
    public function addFamilies($value) { $this->families_[] = $value; }
    public function addAllFamilies(array $values) { foreach($values as $value) {$this->families_[] = $value;} }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.ListFullFamilies)
  }
  
  // message geneweb.api.object.SearchParams
  class SearchParams {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\SearchParams: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->searchType_ = $tmp;
            
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->lastname_ = $tmp;
            $limit-=$len;
            break;
          case 3:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->firstname_ = $tmp;
            $limit-=$len;
            break;
          case 4:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->onlySosa_ = $tmp > 0 ? true : false;
            break;
          case 5:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->onlyRecent_ = $tmp > 0 ? true : false;
            break;
          case 6:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->maidenName_ = $tmp > 0 ? true : false;
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->searchType_)) {
        fwrite($fp, "\x08");
        \Protobuf::write_varint($fp, $this->searchType_);
      }
      if (!is_null($this->lastname_)) {
        fwrite($fp, "\x12");
        \Protobuf::write_varint($fp, strlen($this->lastname_));
        fwrite($fp, $this->lastname_);
      }
      if (!is_null($this->firstname_)) {
        fwrite($fp, "\x1a");
        \Protobuf::write_varint($fp, strlen($this->firstname_));
        fwrite($fp, $this->firstname_);
      }
      if (!is_null($this->onlySosa_)) {
        fwrite($fp, " ");
        \Protobuf::write_varint($fp, $this->onlySosa_ ? 1 : 0);
      }
      if (!is_null($this->onlyRecent_)) {
        fwrite($fp, "(");
        \Protobuf::write_varint($fp, $this->onlyRecent_ ? 1 : 0);
      }
      if (!is_null($this->maidenName_)) {
        fwrite($fp, "0");
        \Protobuf::write_varint($fp, $this->maidenName_ ? 1 : 0);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->searchType_)) {
        $size += 1 + \Protobuf::size_varint($this->searchType_);
      }
      if (!is_null($this->lastname_)) {
        $l = strlen($this->lastname_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->firstname_)) {
        $l = strlen($this->firstname_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->onlySosa_)) {
        $size += 2;
      }
      if (!is_null($this->onlyRecent_)) {
        $size += 2;
      }
      if (!is_null($this->maidenName_)) {
        $size += 2;
      }
      return $size;
    }
    
    public function validateRequired() {
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('searchType_', \geneweb\api\object\SearchType::toString($this->searchType_))
           . \Protobuf::toString('lastname_', $this->lastname_)
           . \Protobuf::toString('firstname_', $this->firstname_)
           . \Protobuf::toString('onlySosa_', $this->onlySosa_)
           . \Protobuf::toString('onlyRecent_', $this->onlyRecent_)
           . \Protobuf::toString('maidenName_', $this->maidenName_);
    }
    
    // optional .geneweb.api.object.SearchType search_type = 1 [default = STARTING_WITH];

    private $searchType_ = null;
    public function clearSearchType() { $this->searchType_ = null; }
    public function hasSearchType() { return $this->searchType_ !== null; }
    public function getSearchType() { if($this->searchType_ === null) return \geneweb\api\object\SearchType::STARTING_WITH; else return $this->searchType_; }
    public function setSearchType($value) { $this->searchType_ = $value; }
    
    // optional string lastname = 2;

    private $lastname_ = null;
    public function clearLastname() { $this->lastname_ = null; }
    public function hasLastname() { return $this->lastname_ !== null; }
    public function getLastname() { if($this->lastname_ === null) return ""; else return $this->lastname_; }
    public function setLastname($value) { $this->lastname_ = $value; }
    
    // optional string firstname = 3;

    private $firstname_ = null;
    public function clearFirstname() { $this->firstname_ = null; }
    public function hasFirstname() { return $this->firstname_ !== null; }
    public function getFirstname() { if($this->firstname_ === null) return ""; else return $this->firstname_; }
    public function setFirstname($value) { $this->firstname_ = $value; }
    
    // optional bool only_sosa = 4 [default = false];

    private $onlySosa_ = null;
    public function clearOnlySosa() { $this->onlySosa_ = null; }
    public function hasOnlySosa() { return $this->onlySosa_ !== null; }
    public function getOnlySosa() { if($this->onlySosa_ === null) return false; else return $this->onlySosa_; }
    public function setOnlySosa($value) { $this->onlySosa_ = $value; }
    
    // optional bool only_recent = 5 [default = false];

    private $onlyRecent_ = null;
    public function clearOnlyRecent() { $this->onlyRecent_ = null; }
    public function hasOnlyRecent() { return $this->onlyRecent_ !== null; }
    public function getOnlyRecent() { if($this->onlyRecent_ === null) return false; else return $this->onlyRecent_; }
    public function setOnlyRecent($value) { $this->onlyRecent_ = $value; }
    
    // optional bool maiden_name = 6 [default = false];

    private $maidenName_ = null;
    public function clearMaidenName() { $this->maidenName_ = null; }
    public function hasMaidenName() { return $this->maidenName_ !== null; }
    public function getMaidenName() { if($this->maidenName_ === null) return false; else return $this->maidenName_; }
    public function setMaidenName($value) { $this->maidenName_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.SearchParams)
  }
  
  // message geneweb.api.object.Image
  class Image {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\Image: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->person_ = new \geneweb\api\object\Person($fp, $len);
            ASSERT('$len == 0');
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->img_ = $tmp;
            $limit-=$len;
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->person_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, $this->person_->size()); // message
        $this->person_->write($fp);
      }
      if (!is_null($this->img_)) {
        fwrite($fp, "\x12");
        \Protobuf::write_varint($fp, strlen($this->img_));
        fwrite($fp, $this->img_);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->person_)) {
        $l = $this->person_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->img_)) {
        $l = strlen($this->img_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->person_ === null) return false;
      if ($this->img_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('person_', $this->person_)
           . \Protobuf::toString('img_', $this->img_);
    }
    
    // required .geneweb.api.object.Person person = 1;

    private $person_ = null;
    public function clearPerson() { $this->person_ = null; }
    public function hasPerson() { return $this->person_ !== null; }
    public function getPerson() { if($this->person_ === null) return null; else return $this->person_; }
    public function setPerson(\geneweb\api\object\Person $value) { $this->person_ = $value; }
    
    // required string img = 2;

    private $img_ = null;
    public function clearImg() { $this->img_ = null; }
    public function hasImg() { return $this->img_ !== null; }
    public function getImg() { if($this->img_ === null) return ""; else return $this->img_; }
    public function setImg($value) { $this->img_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.Image)
  }
  
  // message geneweb.api.object.FullImage
  class FullImage {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\FullImage: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->person_ = new \geneweb\api\object\FullPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->img_ = $tmp;
            $limit-=$len;
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->person_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, $this->person_->size()); // message
        $this->person_->write($fp);
      }
      if (!is_null($this->img_)) {
        fwrite($fp, "\x12");
        \Protobuf::write_varint($fp, strlen($this->img_));
        fwrite($fp, $this->img_);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->person_)) {
        $l = $this->person_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->img_)) {
        $l = strlen($this->img_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->person_ === null) return false;
      if ($this->img_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('person_', $this->person_)
           . \Protobuf::toString('img_', $this->img_);
    }
    
    // required .geneweb.api.object.FullPerson person = 1;

    private $person_ = null;
    public function clearPerson() { $this->person_ = null; }
    public function hasPerson() { return $this->person_ !== null; }
    public function getPerson() { if($this->person_ === null) return null; else return $this->person_; }
    public function setPerson(\geneweb\api\object\FullPerson $value) { $this->person_ = $value; }
    
    // required string img = 2;

    private $img_ = null;
    public function clearImg() { $this->img_ = null; }
    public function hasImg() { return $this->img_ !== null; }
    public function getImg() { if($this->img_ === null) return ""; else return $this->img_; }
    public function setImg($value) { $this->img_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.FullImage)
  }
  
  // message geneweb.api.object.ListImages
  class ListImages {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\ListImages: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->listImages_[] = new \geneweb\api\object\Image($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->listImages_))
        foreach($this->listImages_ as $v) {
          fwrite($fp, "\x0a");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->listImages_))
        foreach($this->listImages_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      return $size;
    }
    
    public function validateRequired() {
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('listImages_', $this->listImages_);
    }
    
    // repeated .geneweb.api.object.Image list_images = 1;

    private $listImages_ = null;
    public function clearListImages() { $this->listImages_ = null; }
    public function getListImagesCount() { if ($this->listImages_ === null ) return 0; else return count($this->listImages_); }
    public function getListImages($index) { return $this->listImages_[$index]; }
    public function getListImagesArray() { if ($this->listImages_ === null ) return array(); else return $this->listImages_; }
    public function setListImages($index, $value) {$this->listImages_[$index] = $value;	}
    public function addListImages($value) { $this->listImages_[] = $value; }
    public function addAllListImages(array $values) { foreach($values as $value) {$this->listImages_[] = $value;} }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.ListImages)
  }
  
  // message geneweb.api.object.ListFullImages
  class ListFullImages {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\ListFullImages: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->images_[] = new \geneweb\api\object\FullImage($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->images_))
        foreach($this->images_ as $v) {
          fwrite($fp, "\x0a");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->images_))
        foreach($this->images_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      return $size;
    }
    
    public function validateRequired() {
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('images_', $this->images_);
    }
    
    // repeated .geneweb.api.object.FullImage images = 1;

    private $images_ = null;
    public function clearImages() { $this->images_ = null; }
    public function getImagesCount() { if ($this->images_ === null ) return 0; else return count($this->images_); }
    public function getImages($index) { return $this->images_[$index]; }
    public function getImagesArray() { if ($this->images_ === null ) return array(); else return $this->images_; }
    public function setImages($index, $value) {$this->images_[$index] = $value;	}
    public function addImages($value) { $this->images_[] = $value; }
    public function addAllImages(array $values) { foreach($values as $value) {$this->images_[] = $value;} }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.ListFullImages)
  }
  
  // message geneweb.api.object.PersImg
  class PersImg {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\PersImg: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->person_ = new \geneweb\api\object\ReferencePerson($fp, $len);
            ASSERT('$len == 0');
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->img_ = $tmp;
            $limit-=$len;
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->person_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, $this->person_->size()); // message
        $this->person_->write($fp);
      }
      if (!is_null($this->img_)) {
        fwrite($fp, "\x12");
        \Protobuf::write_varint($fp, strlen($this->img_));
        fwrite($fp, $this->img_);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->person_)) {
        $l = $this->person_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->img_)) {
        $l = strlen($this->img_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->person_ === null) return false;
      if ($this->img_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('person_', $this->person_)
           . \Protobuf::toString('img_', $this->img_);
    }
    
    // required .geneweb.api.object.ReferencePerson person = 1;

    private $person_ = null;
    public function clearPerson() { $this->person_ = null; }
    public function hasPerson() { return $this->person_ !== null; }
    public function getPerson() { if($this->person_ === null) return null; else return $this->person_; }
    public function setPerson(\geneweb\api\object\ReferencePerson $value) { $this->person_ = $value; }
    
    // required string img = 2;

    private $img_ = null;
    public function clearImg() { $this->img_ = null; }
    public function hasImg() { return $this->img_ !== null; }
    public function getImg() { if($this->img_ === null) return ""; else return $this->img_; }
    public function setImg($value) { $this->img_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.PersImg)
  }
  
  // message geneweb.api.object.ListPersImg
  class ListPersImg {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\ListPersImg: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->listPersImg_[] = new \geneweb\api\object\PersImg($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->listPersImg_))
        foreach($this->listPersImg_ as $v) {
          fwrite($fp, "\x0a");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->listPersImg_))
        foreach($this->listPersImg_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      return $size;
    }
    
    public function validateRequired() {
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('listPersImg_', $this->listPersImg_);
    }
    
    // repeated .geneweb.api.object.PersImg list_pers_img = 1;

    private $listPersImg_ = null;
    public function clearListPersImg() { $this->listPersImg_ = null; }
    public function getListPersImgCount() { if ($this->listPersImg_ === null ) return 0; else return count($this->listPersImg_); }
    public function getListPersImg($index) { return $this->listPersImg_[$index]; }
    public function getListPersImgArray() { if ($this->listPersImg_ === null ) return array(); else return $this->listPersImg_; }
    public function setListPersImg($index, $value) {$this->listPersImg_[$index] = $value;	}
    public function addListPersImg($value) { $this->listPersImg_[] = $value; }
    public function addAllListPersImg(array $values) { foreach($values as $value) {$this->listPersImg_[] = $value;} }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.ListPersImg)
  }
  
  // message geneweb.api.object.Index
  class Index {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\Index: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->index_ = $tmp;
            
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->index_)) {
        fwrite($fp, "\x08");
        \Protobuf::write_varint($fp, $this->index_);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->index_)) {
        $size += 1 + \Protobuf::size_varint($this->index_);
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->index_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('index_', $this->index_);
    }
    
    // required int32 index = 1;

    private $index_ = null;
    public function clearIndex() { $this->index_ = null; }
    public function hasIndex() { return $this->index_ !== null; }
    public function getIndex() { if($this->index_ === null) return 0; else return $this->index_; }
    public function setIndex($value) { $this->index_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.Index)
  }
  
  // message geneweb.api.object.ImageAddress
  class ImageAddress {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\ImageAddress: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->img_ = $tmp;
            $limit-=$len;
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->img_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, strlen($this->img_));
        fwrite($fp, $this->img_);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->img_)) {
        $l = strlen($this->img_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->img_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('img_', $this->img_);
    }
    
    // required string img = 1;

    private $img_ = null;
    public function clearImg() { $this->img_ = null; }
    public function hasImg() { return $this->img_ !== null; }
    public function getImg() { if($this->img_ === null) return ""; else return $this->img_; }
    public function setImg($value) { $this->img_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.ImageAddress)
  }
  
  // message geneweb.api.object.ClosePersonsParams
  class ClosePersonsParams {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\ClosePersonsParams: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->person_ = new \geneweb\api\object\ReferencePerson($fp, $len);
            ASSERT('$len == 0');
            break;
          case 2:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->nbGenAsc_ = $tmp;
            
            break;
          case 3:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->nbGenDesc_ = $tmp;
            
            break;
          case 4:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->spouseAscend_ = $tmp > 0 ? true : false;
            break;
          case 5:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->onlyRecent_ = $tmp > 0 ? true : false;
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->person_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, $this->person_->size()); // message
        $this->person_->write($fp);
      }
      if (!is_null($this->nbGenAsc_)) {
        fwrite($fp, "\x10");
        \Protobuf::write_varint($fp, $this->nbGenAsc_);
      }
      if (!is_null($this->nbGenDesc_)) {
        fwrite($fp, "\x18");
        \Protobuf::write_varint($fp, $this->nbGenDesc_);
      }
      if (!is_null($this->spouseAscend_)) {
        fwrite($fp, " ");
        \Protobuf::write_varint($fp, $this->spouseAscend_ ? 1 : 0);
      }
      if (!is_null($this->onlyRecent_)) {
        fwrite($fp, "(");
        \Protobuf::write_varint($fp, $this->onlyRecent_ ? 1 : 0);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->person_)) {
        $l = $this->person_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->nbGenAsc_)) {
        $size += 1 + \Protobuf::size_varint($this->nbGenAsc_);
      }
      if (!is_null($this->nbGenDesc_)) {
        $size += 1 + \Protobuf::size_varint($this->nbGenDesc_);
      }
      if (!is_null($this->spouseAscend_)) {
        $size += 2;
      }
      if (!is_null($this->onlyRecent_)) {
        $size += 2;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->person_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('person_', $this->person_)
           . \Protobuf::toString('nbGenAsc_', $this->nbGenAsc_)
           . \Protobuf::toString('nbGenDesc_', $this->nbGenDesc_)
           . \Protobuf::toString('spouseAscend_', $this->spouseAscend_)
           . \Protobuf::toString('onlyRecent_', $this->onlyRecent_);
    }
    
    // required .geneweb.api.object.ReferencePerson person = 1;

    private $person_ = null;
    public function clearPerson() { $this->person_ = null; }
    public function hasPerson() { return $this->person_ !== null; }
    public function getPerson() { if($this->person_ === null) return null; else return $this->person_; }
    public function setPerson(\geneweb\api\object\ReferencePerson $value) { $this->person_ = $value; }
    
    // optional int32 nb_gen_asc = 2;

    private $nbGenAsc_ = null;
    public function clearNbGenAsc() { $this->nbGenAsc_ = null; }
    public function hasNbGenAsc() { return $this->nbGenAsc_ !== null; }
    public function getNbGenAsc() { if($this->nbGenAsc_ === null) return 0; else return $this->nbGenAsc_; }
    public function setNbGenAsc($value) { $this->nbGenAsc_ = $value; }
    
    // optional int32 nb_gen_desc = 3;

    private $nbGenDesc_ = null;
    public function clearNbGenDesc() { $this->nbGenDesc_ = null; }
    public function hasNbGenDesc() { return $this->nbGenDesc_ !== null; }
    public function getNbGenDesc() { if($this->nbGenDesc_ === null) return 0; else return $this->nbGenDesc_; }
    public function setNbGenDesc($value) { $this->nbGenDesc_ = $value; }
    
    // optional bool spouse_ascend = 4 [default = false];

    private $spouseAscend_ = null;
    public function clearSpouseAscend() { $this->spouseAscend_ = null; }
    public function hasSpouseAscend() { return $this->spouseAscend_ !== null; }
    public function getSpouseAscend() { if($this->spouseAscend_ === null) return false; else return $this->spouseAscend_; }
    public function setSpouseAscend($value) { $this->spouseAscend_ = $value; }
    
    // optional bool only_recent = 5 [default = false];

    private $onlyRecent_ = null;
    public function clearOnlyRecent() { $this->onlyRecent_ = null; }
    public function hasOnlyRecent() { return $this->onlyRecent_ !== null; }
    public function getOnlyRecent() { if($this->onlyRecent_ === null) return false; else return $this->onlyRecent_; }
    public function setOnlyRecent($value) { $this->onlyRecent_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.ClosePersonsParams)
  }
  
  // message geneweb.api.object.PersonRelation
  class PersonRelation {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\PersonRelation: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->person_ = new \geneweb\api\object\Person($fp, $len);
            ASSERT('$len == 0');
            break;
          case 2:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->relation_ = $tmp;
            
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->person_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, $this->person_->size()); // message
        $this->person_->write($fp);
      }
      if (!is_null($this->relation_)) {
        fwrite($fp, "\x10");
        \Protobuf::write_varint($fp, $this->relation_);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->person_)) {
        $l = $this->person_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->relation_)) {
        $size += 1 + \Protobuf::size_varint($this->relation_);
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->person_ === null) return false;
      if ($this->relation_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('person_', $this->person_)
           . \Protobuf::toString('relation_', \geneweb\api\object\RelationType::toString($this->relation_));
    }
    
    // required .geneweb.api.object.Person person = 1;

    private $person_ = null;
    public function clearPerson() { $this->person_ = null; }
    public function hasPerson() { return $this->person_ !== null; }
    public function getPerson() { if($this->person_ === null) return null; else return $this->person_; }
    public function setPerson(\geneweb\api\object\Person $value) { $this->person_ = $value; }
    
    // required .geneweb.api.object.RelationType relation = 2;

    private $relation_ = null;
    public function clearRelation() { $this->relation_ = null; }
    public function hasRelation() { return $this->relation_ !== null; }
    public function getRelation() { if($this->relation_ === null) return \geneweb\api\object\RelationType::SELF; else return $this->relation_; }
    public function setRelation($value) { $this->relation_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.PersonRelation)
  }
  
  // message geneweb.api.object.FullPersonRelation
  class FullPersonRelation {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\FullPersonRelation: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->person_ = new \geneweb\api\object\FullPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          case 2:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->relation_ = $tmp;
            
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->person_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, $this->person_->size()); // message
        $this->person_->write($fp);
      }
      if (!is_null($this->relation_)) {
        fwrite($fp, "\x10");
        \Protobuf::write_varint($fp, $this->relation_);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->person_)) {
        $l = $this->person_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->relation_)) {
        $size += 1 + \Protobuf::size_varint($this->relation_);
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->person_ === null) return false;
      if ($this->relation_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('person_', $this->person_)
           . \Protobuf::toString('relation_', \geneweb\api\object\RelationType::toString($this->relation_));
    }
    
    // required .geneweb.api.object.FullPerson person = 1;

    private $person_ = null;
    public function clearPerson() { $this->person_ = null; }
    public function hasPerson() { return $this->person_ !== null; }
    public function getPerson() { if($this->person_ === null) return null; else return $this->person_; }
    public function setPerson(\geneweb\api\object\FullPerson $value) { $this->person_ = $value; }
    
    // required .geneweb.api.object.RelationType relation = 2;

    private $relation_ = null;
    public function clearRelation() { $this->relation_ = null; }
    public function hasRelation() { return $this->relation_ !== null; }
    public function getRelation() { if($this->relation_ === null) return \geneweb\api\object\RelationType::SELF; else return $this->relation_; }
    public function setRelation($value) { $this->relation_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.FullPersonRelation)
  }
  
  // message geneweb.api.object.ListPersonRelation
  class ListPersonRelation {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\ListPersonRelation: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->personRelations_[] = new \geneweb\api\object\PersonRelation($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->personRelations_))
        foreach($this->personRelations_ as $v) {
          fwrite($fp, "\x0a");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->personRelations_))
        foreach($this->personRelations_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      return $size;
    }
    
    public function validateRequired() {
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('personRelations_', $this->personRelations_);
    }
    
    // repeated .geneweb.api.object.PersonRelation person_relations = 1;

    private $personRelations_ = null;
    public function clearPersonRelations() { $this->personRelations_ = null; }
    public function getPersonRelationsCount() { if ($this->personRelations_ === null ) return 0; else return count($this->personRelations_); }
    public function getPersonRelations($index) { return $this->personRelations_[$index]; }
    public function getPersonRelationsArray() { if ($this->personRelations_ === null ) return array(); else return $this->personRelations_; }
    public function setPersonRelations($index, $value) {$this->personRelations_[$index] = $value;	}
    public function addPersonRelations($value) { $this->personRelations_[] = $value; }
    public function addAllPersonRelations(array $values) { foreach($values as $value) {$this->personRelations_[] = $value;} }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.ListPersonRelation)
  }
  
  // message geneweb.api.object.ListFullPersonRelation
  class ListFullPersonRelation {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\ListFullPersonRelation: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->personRelations_[] = new \geneweb\api\object\FullPersonRelation($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->personRelations_))
        foreach($this->personRelations_ as $v) {
          fwrite($fp, "\x0a");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->personRelations_))
        foreach($this->personRelations_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      return $size;
    }
    
    public function validateRequired() {
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('personRelations_', $this->personRelations_);
    }
    
    // repeated .geneweb.api.object.FullPersonRelation person_relations = 1;

    private $personRelations_ = null;
    public function clearPersonRelations() { $this->personRelations_ = null; }
    public function getPersonRelationsCount() { if ($this->personRelations_ === null ) return 0; else return count($this->personRelations_); }
    public function getPersonRelations($index) { return $this->personRelations_[$index]; }
    public function getPersonRelationsArray() { if ($this->personRelations_ === null ) return array(); else return $this->personRelations_; }
    public function setPersonRelations($index, $value) {$this->personRelations_[$index] = $value;	}
    public function addPersonRelations($value) { $this->personRelations_[] = $value; }
    public function addAllPersonRelations(array $values) { foreach($values as $value) {$this->personRelations_[] = $value;} }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.ListFullPersonRelation)
  }
  
  // message geneweb.api.object.AnniversaryParams
  class AnniversaryParams {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\AnniversaryParams: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->month_ = $tmp;
            
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->month_)) {
        fwrite($fp, "\x08");
        \Protobuf::write_varint($fp, $this->month_);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->month_)) {
        $size += 1 + \Protobuf::size_varint($this->month_);
      }
      return $size;
    }
    
    public function validateRequired() {
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('month_', $this->month_);
    }
    
    // optional int32 month = 1;

    private $month_ = null;
    public function clearMonth() { $this->month_ = null; }
    public function hasMonth() { return $this->month_ !== null; }
    public function getMonth() { if($this->month_ === null) return 0; else return $this->month_; }
    public function setMonth($value) { $this->month_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.AnniversaryParams)
  }
  
  // message geneweb.api.object.GraphParams
  class GraphParams {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\GraphParams: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->generation_ = $tmp;
            
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->person_ = new \geneweb\api\object\ReferencePerson($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->generation_)) {
        fwrite($fp, "\x08");
        \Protobuf::write_varint($fp, $this->generation_);
      }
      if (!is_null($this->person_)) {
        fwrite($fp, "\x12");
        \Protobuf::write_varint($fp, $this->person_->size()); // message
        $this->person_->write($fp);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->generation_)) {
        $size += 1 + \Protobuf::size_varint($this->generation_);
      }
      if (!is_null($this->person_)) {
        $l = $this->person_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->person_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('generation_', $this->generation_)
           . \Protobuf::toString('person_', $this->person_);
    }
    
    // optional int32 generation = 1;

    private $generation_ = null;
    public function clearGeneration() { $this->generation_ = null; }
    public function hasGeneration() { return $this->generation_ !== null; }
    public function getGeneration() { if($this->generation_ === null) return 0; else return $this->generation_; }
    public function setGeneration($value) { $this->generation_ = $value; }
    
    // required .geneweb.api.object.ReferencePerson person = 2;

    private $person_ = null;
    public function clearPerson() { $this->person_ = null; }
    public function hasPerson() { return $this->person_ !== null; }
    public function getPerson() { if($this->person_ === null) return null; else return $this->person_; }
    public function setPerson(\geneweb\api\object\ReferencePerson $value) { $this->person_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.GraphParams)
  }
  
  // message geneweb.api.object.GraphRelParams
  class GraphRelParams {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\GraphRelParams: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->person1_ = new \geneweb\api\object\ReferencePerson($fp, $len);
            ASSERT('$len == 0');
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->person2_ = new \geneweb\api\object\ReferencePerson($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->person1_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, $this->person1_->size()); // message
        $this->person1_->write($fp);
      }
      if (!is_null($this->person2_)) {
        fwrite($fp, "\x12");
        \Protobuf::write_varint($fp, $this->person2_->size()); // message
        $this->person2_->write($fp);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->person1_)) {
        $l = $this->person1_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->person2_)) {
        $l = $this->person2_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->person1_ === null) return false;
      if ($this->person2_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('person1_', $this->person1_)
           . \Protobuf::toString('person2_', $this->person2_);
    }
    
    // required .geneweb.api.object.ReferencePerson person1 = 1;

    private $person1_ = null;
    public function clearPerson1() { $this->person1_ = null; }
    public function hasPerson1() { return $this->person1_ !== null; }
    public function getPerson1() { if($this->person1_ === null) return null; else return $this->person1_; }
    public function setPerson1(\geneweb\api\object\ReferencePerson $value) { $this->person1_ = $value; }
    
    // required .geneweb.api.object.ReferencePerson person2 = 2;

    private $person2_ = null;
    public function clearPerson2() { $this->person2_ = null; }
    public function hasPerson2() { return $this->person2_ !== null; }
    public function getPerson2() { if($this->person2_ === null) return null; else return $this->person2_; }
    public function setPerson2(\geneweb\api\object\ReferencePerson $value) { $this->person2_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.GraphRelParams)
  }
  
  // message geneweb.api.object.CplRelParams
  class CplRelParams {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\CplRelParams: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->person1_ = new \geneweb\api\object\ReferencePerson($fp, $len);
            ASSERT('$len == 0');
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->person2_ = new \geneweb\api\object\ReferencePerson($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->person1_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, $this->person1_->size()); // message
        $this->person1_->write($fp);
      }
      if (!is_null($this->person2_)) {
        fwrite($fp, "\x12");
        \Protobuf::write_varint($fp, $this->person2_->size()); // message
        $this->person2_->write($fp);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->person1_)) {
        $l = $this->person1_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->person2_)) {
        $l = $this->person2_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->person1_ === null) return false;
      if ($this->person2_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('person1_', $this->person1_)
           . \Protobuf::toString('person2_', $this->person2_);
    }
    
    // required .geneweb.api.object.ReferencePerson person1 = 1;

    private $person1_ = null;
    public function clearPerson1() { $this->person1_ = null; }
    public function hasPerson1() { return $this->person1_ !== null; }
    public function getPerson1() { if($this->person1_ === null) return null; else return $this->person1_; }
    public function setPerson1(\geneweb\api\object\ReferencePerson $value) { $this->person1_ = $value; }
    
    // required .geneweb.api.object.ReferencePerson person2 = 2;

    private $person2_ = null;
    public function clearPerson2() { $this->person2_ = null; }
    public function hasPerson2() { return $this->person2_ !== null; }
    public function getPerson2() { if($this->person2_ === null) return null; else return $this->person2_; }
    public function setPerson2(\geneweb\api\object\ReferencePerson $value) { $this->person2_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.CplRelParams)
  }
  
  // message geneweb.api.object.Node
  class Node {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\Node: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->id_ = $tmp;
            
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->person_ = new \geneweb\api\object\Person($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->id_)) {
        fwrite($fp, "\x08");
        \Protobuf::write_varint($fp, $this->id_);
      }
      if (!is_null($this->person_)) {
        fwrite($fp, "\x12");
        \Protobuf::write_varint($fp, $this->person_->size()); // message
        $this->person_->write($fp);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->id_)) {
        $size += 1 + \Protobuf::size_varint($this->id_);
      }
      if (!is_null($this->person_)) {
        $l = $this->person_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->id_ === null) return false;
      if ($this->person_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('id_', $this->id_)
           . \Protobuf::toString('person_', $this->person_);
    }
    
    // required int64 id = 1;

    private $id_ = null;
    public function clearId() { $this->id_ = null; }
    public function hasId() { return $this->id_ !== null; }
    public function getId() { if($this->id_ === null) return 0; else return $this->id_; }
    public function setId($value) { $this->id_ = $value; }
    
    // required .geneweb.api.object.Person person = 2;

    private $person_ = null;
    public function clearPerson() { $this->person_ = null; }
    public function hasPerson() { return $this->person_ !== null; }
    public function getPerson() { if($this->person_ === null) return null; else return $this->person_; }
    public function setPerson(\geneweb\api\object\Person $value) { $this->person_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.Node)
  }
  
  // message geneweb.api.object.FullNode
  class FullNode {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\FullNode: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->id_ = $tmp;
            
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->person_ = new \geneweb\api\object\FullPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->id_)) {
        fwrite($fp, "\x08");
        \Protobuf::write_varint($fp, $this->id_);
      }
      if (!is_null($this->person_)) {
        fwrite($fp, "\x12");
        \Protobuf::write_varint($fp, $this->person_->size()); // message
        $this->person_->write($fp);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->id_)) {
        $size += 1 + \Protobuf::size_varint($this->id_);
      }
      if (!is_null($this->person_)) {
        $l = $this->person_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->id_ === null) return false;
      if ($this->person_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('id_', $this->id_)
           . \Protobuf::toString('person_', $this->person_);
    }
    
    // required int64 id = 1;

    private $id_ = null;
    public function clearId() { $this->id_ = null; }
    public function hasId() { return $this->id_ !== null; }
    public function getId() { if($this->id_ === null) return 0; else return $this->id_; }
    public function setId($value) { $this->id_ = $value; }
    
    // required .geneweb.api.object.FullPerson person = 2;

    private $person_ = null;
    public function clearPerson() { $this->person_ = null; }
    public function hasPerson() { return $this->person_ !== null; }
    public function getPerson() { if($this->person_ === null) return null; else return $this->person_; }
    public function setPerson(\geneweb\api\object\FullPerson $value) { $this->person_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.FullNode)
  }
  
  // message geneweb.api.object.Edge
  class Edge {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\Edge: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->fromNode_ = $tmp;
            
            break;
          case 2:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->toNode_ = $tmp;
            
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->fromNode_)) {
        fwrite($fp, "\x08");
        \Protobuf::write_varint($fp, $this->fromNode_);
      }
      if (!is_null($this->toNode_)) {
        fwrite($fp, "\x10");
        \Protobuf::write_varint($fp, $this->toNode_);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->fromNode_)) {
        $size += 1 + \Protobuf::size_varint($this->fromNode_);
      }
      if (!is_null($this->toNode_)) {
        $size += 1 + \Protobuf::size_varint($this->toNode_);
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->fromNode_ === null) return false;
      if ($this->toNode_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('fromNode_', $this->fromNode_)
           . \Protobuf::toString('toNode_', $this->toNode_);
    }
    
    // required int64 from_node = 1;

    private $fromNode_ = null;
    public function clearFromNode() { $this->fromNode_ = null; }
    public function hasFromNode() { return $this->fromNode_ !== null; }
    public function getFromNode() { if($this->fromNode_ === null) return 0; else return $this->fromNode_; }
    public function setFromNode($value) { $this->fromNode_ = $value; }
    
    // required int64 to_node = 2;

    private $toNode_ = null;
    public function clearToNode() { $this->toNode_ = null; }
    public function hasToNode() { return $this->toNode_ !== null; }
    public function getToNode() { if($this->toNode_ === null) return 0; else return $this->toNode_; }
    public function setToNode($value) { $this->toNode_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.Edge)
  }
  
  // message geneweb.api.object.Graph
  class Graph {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\Graph: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->nodes_[] = new \geneweb\api\object\Node($fp, $len);
            ASSERT('$len == 0');
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->edges_[] = new \geneweb\api\object\Edge($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->nodes_))
        foreach($this->nodes_ as $v) {
          fwrite($fp, "\x0a");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->edges_))
        foreach($this->edges_ as $v) {
          fwrite($fp, "\x12");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->nodes_))
        foreach($this->nodes_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->edges_))
        foreach($this->edges_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      return $size;
    }
    
    public function validateRequired() {
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('nodes_', $this->nodes_)
           . \Protobuf::toString('edges_', $this->edges_);
    }
    
    // repeated .geneweb.api.object.Node nodes = 1;

    private $nodes_ = null;
    public function clearNodes() { $this->nodes_ = null; }
    public function getNodesCount() { if ($this->nodes_ === null ) return 0; else return count($this->nodes_); }
    public function getNodes($index) { return $this->nodes_[$index]; }
    public function getNodesArray() { if ($this->nodes_ === null ) return array(); else return $this->nodes_; }
    public function setNodes($index, $value) {$this->nodes_[$index] = $value;	}
    public function addNodes($value) { $this->nodes_[] = $value; }
    public function addAllNodes(array $values) { foreach($values as $value) {$this->nodes_[] = $value;} }
    
    // repeated .geneweb.api.object.Edge edges = 2;

    private $edges_ = null;
    public function clearEdges() { $this->edges_ = null; }
    public function getEdgesCount() { if ($this->edges_ === null ) return 0; else return count($this->edges_); }
    public function getEdges($index) { return $this->edges_[$index]; }
    public function getEdgesArray() { if ($this->edges_ === null ) return array(); else return $this->edges_; }
    public function setEdges($index, $value) {$this->edges_[$index] = $value;	}
    public function addEdges($value) { $this->edges_[] = $value; }
    public function addAllEdges(array $values) { foreach($values as $value) {$this->edges_[] = $value;} }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.Graph)
  }
  
  // message geneweb.api.object.FullGraph
  class FullGraph {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\FullGraph: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->nodes_[] = new \geneweb\api\object\FullNode($fp, $len);
            ASSERT('$len == 0');
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->edges_[] = new \geneweb\api\object\Edge($fp, $len);
            ASSERT('$len == 0');
            break;
          case 3:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->families_[] = new \geneweb\api\object\FullFamily($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->nodes_))
        foreach($this->nodes_ as $v) {
          fwrite($fp, "\x0a");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->edges_))
        foreach($this->edges_ as $v) {
          fwrite($fp, "\x12");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->families_))
        foreach($this->families_ as $v) {
          fwrite($fp, "\x1a");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->nodes_))
        foreach($this->nodes_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->edges_))
        foreach($this->edges_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->families_))
        foreach($this->families_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      return $size;
    }
    
    public function validateRequired() {
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('nodes_', $this->nodes_)
           . \Protobuf::toString('edges_', $this->edges_)
           . \Protobuf::toString('families_', $this->families_);
    }
    
    // repeated .geneweb.api.object.FullNode nodes = 1;

    private $nodes_ = null;
    public function clearNodes() { $this->nodes_ = null; }
    public function getNodesCount() { if ($this->nodes_ === null ) return 0; else return count($this->nodes_); }
    public function getNodes($index) { return $this->nodes_[$index]; }
    public function getNodesArray() { if ($this->nodes_ === null ) return array(); else return $this->nodes_; }
    public function setNodes($index, $value) {$this->nodes_[$index] = $value;	}
    public function addNodes($value) { $this->nodes_[] = $value; }
    public function addAllNodes(array $values) { foreach($values as $value) {$this->nodes_[] = $value;} }
    
    // repeated .geneweb.api.object.Edge edges = 2;

    private $edges_ = null;
    public function clearEdges() { $this->edges_ = null; }
    public function getEdgesCount() { if ($this->edges_ === null ) return 0; else return count($this->edges_); }
    public function getEdges($index) { return $this->edges_[$index]; }
    public function getEdgesArray() { if ($this->edges_ === null ) return array(); else return $this->edges_; }
    public function setEdges($index, $value) {$this->edges_[$index] = $value;	}
    public function addEdges($value) { $this->edges_[] = $value; }
    public function addAllEdges(array $values) { foreach($values as $value) {$this->edges_[] = $value;} }
    
    // repeated .geneweb.api.object.FullFamily families = 3;

    private $families_ = null;
    public function clearFamilies() { $this->families_ = null; }
    public function getFamiliesCount() { if ($this->families_ === null ) return 0; else return count($this->families_); }
    public function getFamilies($index) { return $this->families_[$index]; }
    public function getFamiliesArray() { if ($this->families_ === null ) return array(); else return $this->families_; }
    public function setFamilies($index, $value) {$this->families_[$index] = $value;	}
    public function addFamilies($value) { $this->families_[] = $value; }
    public function addAllFamilies(array $values) { foreach($values as $value) {$this->families_[] = $value;} }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.FullGraph)
  }
  
  // message geneweb.api.object.AllPersonsParams
  class AllPersonsParams {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\AllPersonsParams: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->from_ = $tmp;
            
            break;
          case 2:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->limit_ = $tmp;
            
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->from_)) {
        fwrite($fp, "\x08");
        \Protobuf::write_varint($fp, $this->from_);
      }
      if (!is_null($this->limit_)) {
        fwrite($fp, "\x10");
        \Protobuf::write_varint($fp, $this->limit_);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->from_)) {
        $size += 1 + \Protobuf::size_varint($this->from_);
      }
      if (!is_null($this->limit_)) {
        $size += 1 + \Protobuf::size_varint($this->limit_);
      }
      return $size;
    }
    
    public function validateRequired() {
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('from_', $this->from_)
           . \Protobuf::toString('limit_', $this->limit_);
    }
    
    // optional int32 from = 1;

    private $from_ = null;
    public function clearFrom() { $this->from_ = null; }
    public function hasFrom() { return $this->from_ !== null; }
    public function getFrom() { if($this->from_ === null) return 0; else return $this->from_; }
    public function setFrom($value) { $this->from_ = $value; }
    
    // optional int32 limit = 2;

    private $limit_ = null;
    public function clearLimit() { $this->limit_ = null; }
    public function hasLimit() { return $this->limit_ !== null; }
    public function getLimit() { if($this->limit_ === null) return 0; else return $this->limit_; }
    public function setLimit($value) { $this->limit_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.AllPersonsParams)
  }
  
  // message geneweb.api.object.AllFamiliesParams
  class AllFamiliesParams {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\AllFamiliesParams: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->from_ = $tmp;
            
            break;
          case 2:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->limit_ = $tmp;
            
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->from_)) {
        fwrite($fp, "\x08");
        \Protobuf::write_varint($fp, $this->from_);
      }
      if (!is_null($this->limit_)) {
        fwrite($fp, "\x10");
        \Protobuf::write_varint($fp, $this->limit_);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->from_)) {
        $size += 1 + \Protobuf::size_varint($this->from_);
      }
      if (!is_null($this->limit_)) {
        $size += 1 + \Protobuf::size_varint($this->limit_);
      }
      return $size;
    }
    
    public function validateRequired() {
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('from_', $this->from_)
           . \Protobuf::toString('limit_', $this->limit_);
    }
    
    // optional int32 from = 1;

    private $from_ = null;
    public function clearFrom() { $this->from_ = null; }
    public function hasFrom() { return $this->from_ !== null; }
    public function getFrom() { if($this->from_ === null) return 0; else return $this->from_; }
    public function setFrom($value) { $this->from_ = $value; }
    
    // optional int32 limit = 2;

    private $limit_ = null;
    public function clearLimit() { $this->limit_ = null; }
    public function hasLimit() { return $this->limit_ !== null; }
    public function getLimit() { if($this->limit_ === null) return 0; else return $this->limit_; }
    public function setLimit($value) { $this->limit_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.AllFamiliesParams)
  }
  
  // message geneweb.api.object.WarningAlreadyDefined
  class WarningAlreadyDefined {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\WarningAlreadyDefined: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->person_ = new \geneweb\api\object\FullPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->person_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, $this->person_->size()); // message
        $this->person_->write($fp);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->person_)) {
        $l = $this->person_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->person_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('person_', $this->person_);
    }
    
    // required .geneweb.api.object.FullPerson person = 1;

    private $person_ = null;
    public function clearPerson() { $this->person_ = null; }
    public function hasPerson() { return $this->person_ !== null; }
    public function getPerson() { if($this->person_ === null) return null; else return $this->person_; }
    public function setPerson(\geneweb\api\object\FullPerson $value) { $this->person_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.WarningAlreadyDefined)
  }
  
  // message geneweb.api.object.WarningOwnAncestor
  class WarningOwnAncestor {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\WarningOwnAncestor: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->person_ = new \geneweb\api\object\FullPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->person_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, $this->person_->size()); // message
        $this->person_->write($fp);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->person_)) {
        $l = $this->person_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->person_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('person_', $this->person_);
    }
    
    // required .geneweb.api.object.FullPerson person = 1;

    private $person_ = null;
    public function clearPerson() { $this->person_ = null; }
    public function hasPerson() { return $this->person_ !== null; }
    public function getPerson() { if($this->person_ === null) return null; else return $this->person_; }
    public function setPerson(\geneweb\api\object\FullPerson $value) { $this->person_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.WarningOwnAncestor)
  }
  
  // message geneweb.api.object.WarningBadSexOfMarriedPerson
  class WarningBadSexOfMarriedPerson {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\WarningBadSexOfMarriedPerson: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->person_ = new \geneweb\api\object\FullPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->person_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, $this->person_->size()); // message
        $this->person_->write($fp);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->person_)) {
        $l = $this->person_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->person_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('person_', $this->person_);
    }
    
    // required .geneweb.api.object.FullPerson person = 1;

    private $person_ = null;
    public function clearPerson() { $this->person_ = null; }
    public function hasPerson() { return $this->person_ !== null; }
    public function getPerson() { if($this->person_ === null) return null; else return $this->person_; }
    public function setPerson(\geneweb\api\object\FullPerson $value) { $this->person_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.WarningBadSexOfMarriedPerson)
  }
  
  // message geneweb.api.object.WarningBirthAfterDeath
  class WarningBirthAfterDeath {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\WarningBirthAfterDeath: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->person_ = new \geneweb\api\object\FullPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->person_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, $this->person_->size()); // message
        $this->person_->write($fp);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->person_)) {
        $l = $this->person_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->person_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('person_', $this->person_);
    }
    
    // required .geneweb.api.object.FullPerson person = 1;

    private $person_ = null;
    public function clearPerson() { $this->person_ = null; }
    public function hasPerson() { return $this->person_ !== null; }
    public function getPerson() { if($this->person_ === null) return null; else return $this->person_; }
    public function setPerson(\geneweb\api\object\FullPerson $value) { $this->person_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.WarningBirthAfterDeath)
  }
  
  // message geneweb.api.object.WarningIncoherentSex
  class WarningIncoherentSex {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\WarningIncoherentSex: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->person_ = new \geneweb\api\object\FullPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->person_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, $this->person_->size()); // message
        $this->person_->write($fp);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->person_)) {
        $l = $this->person_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->person_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('person_', $this->person_);
    }
    
    // required .geneweb.api.object.FullPerson person = 1;

    private $person_ = null;
    public function clearPerson() { $this->person_ = null; }
    public function hasPerson() { return $this->person_ !== null; }
    public function getPerson() { if($this->person_ === null) return null; else return $this->person_; }
    public function setPerson(\geneweb\api\object\FullPerson $value) { $this->person_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.WarningIncoherentSex)
  }
  
  // message geneweb.api.object.WarningChangedOrderOfChildren
  class WarningChangedOrderOfChildren {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\WarningChangedOrderOfChildren: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->father_ = new \geneweb\api\object\FullPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->mother_ = new \geneweb\api\object\FullPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->father_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, $this->father_->size()); // message
        $this->father_->write($fp);
      }
      if (!is_null($this->mother_)) {
        fwrite($fp, "\x12");
        \Protobuf::write_varint($fp, $this->mother_->size()); // message
        $this->mother_->write($fp);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->father_)) {
        $l = $this->father_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->mother_)) {
        $l = $this->mother_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->father_ === null) return false;
      if ($this->mother_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('father_', $this->father_)
           . \Protobuf::toString('mother_', $this->mother_);
    }
    
    // required .geneweb.api.object.FullPerson father = 1;

    private $father_ = null;
    public function clearFather() { $this->father_ = null; }
    public function hasFather() { return $this->father_ !== null; }
    public function getFather() { if($this->father_ === null) return null; else return $this->father_; }
    public function setFather(\geneweb\api\object\FullPerson $value) { $this->father_ = $value; }
    
    // required .geneweb.api.object.FullPerson mother = 2;

    private $mother_ = null;
    public function clearMother() { $this->mother_ = null; }
    public function hasMother() { return $this->mother_ !== null; }
    public function getMother() { if($this->mother_ === null) return null; else return $this->mother_; }
    public function setMother(\geneweb\api\object\FullPerson $value) { $this->mother_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.WarningChangedOrderOfChildren)
  }
  
  // message geneweb.api.object.WarningChangedOrderOfMarriages
  class WarningChangedOrderOfMarriages {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\WarningChangedOrderOfMarriages: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->person_ = new \geneweb\api\object\FullPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->person_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, $this->person_->size()); // message
        $this->person_->write($fp);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->person_)) {
        $l = $this->person_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->person_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('person_', $this->person_);
    }
    
    // required .geneweb.api.object.FullPerson person = 1;

    private $person_ = null;
    public function clearPerson() { $this->person_ = null; }
    public function hasPerson() { return $this->person_ !== null; }
    public function getPerson() { if($this->person_ === null) return null; else return $this->person_; }
    public function setPerson(\geneweb\api\object\FullPerson $value) { $this->person_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.WarningChangedOrderOfMarriages)
  }
  
  // message geneweb.api.object.WarningChildrenNotInOrder
  class WarningChildrenNotInOrder {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\WarningChildrenNotInOrder: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->father_ = new \geneweb\api\object\FullPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->mother_ = new \geneweb\api\object\FullPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->father_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, $this->father_->size()); // message
        $this->father_->write($fp);
      }
      if (!is_null($this->mother_)) {
        fwrite($fp, "\x12");
        \Protobuf::write_varint($fp, $this->mother_->size()); // message
        $this->mother_->write($fp);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->father_)) {
        $l = $this->father_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->mother_)) {
        $l = $this->mother_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->father_ === null) return false;
      if ($this->mother_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('father_', $this->father_)
           . \Protobuf::toString('mother_', $this->mother_);
    }
    
    // required .geneweb.api.object.FullPerson father = 1;

    private $father_ = null;
    public function clearFather() { $this->father_ = null; }
    public function hasFather() { return $this->father_ !== null; }
    public function getFather() { if($this->father_ === null) return null; else return $this->father_; }
    public function setFather(\geneweb\api\object\FullPerson $value) { $this->father_ = $value; }
    
    // required .geneweb.api.object.FullPerson mother = 2;

    private $mother_ = null;
    public function clearMother() { $this->mother_ = null; }
    public function hasMother() { return $this->mother_ !== null; }
    public function getMother() { if($this->mother_ === null) return null; else return $this->mother_; }
    public function setMother(\geneweb\api\object\FullPerson $value) { $this->mother_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.WarningChildrenNotInOrder)
  }
  
  // message geneweb.api.object.WarningDeadTooEarlyToBeFather
  class WarningDeadTooEarlyToBeFather {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\WarningDeadTooEarlyToBeFather: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->son_ = new \geneweb\api\object\FullPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->father_ = new \geneweb\api\object\FullPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->son_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, $this->son_->size()); // message
        $this->son_->write($fp);
      }
      if (!is_null($this->father_)) {
        fwrite($fp, "\x12");
        \Protobuf::write_varint($fp, $this->father_->size()); // message
        $this->father_->write($fp);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->son_)) {
        $l = $this->son_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->father_)) {
        $l = $this->father_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->son_ === null) return false;
      if ($this->father_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('son_', $this->son_)
           . \Protobuf::toString('father_', $this->father_);
    }
    
    // required .geneweb.api.object.FullPerson son = 1;

    private $son_ = null;
    public function clearSon() { $this->son_ = null; }
    public function hasSon() { return $this->son_ !== null; }
    public function getSon() { if($this->son_ === null) return null; else return $this->son_; }
    public function setSon(\geneweb\api\object\FullPerson $value) { $this->son_ = $value; }
    
    // required .geneweb.api.object.FullPerson father = 2;

    private $father_ = null;
    public function clearFather() { $this->father_ = null; }
    public function hasFather() { return $this->father_ !== null; }
    public function getFather() { if($this->father_ === null) return null; else return $this->father_; }
    public function setFather(\geneweb\api\object\FullPerson $value) { $this->father_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.WarningDeadTooEarlyToBeFather)
  }
  
  // message geneweb.api.object.WarningIncoherentAncestorDate
  class WarningIncoherentAncestorDate {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\WarningIncoherentAncestorDate: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->person_ = new \geneweb\api\object\FullPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->ancestor_ = new \geneweb\api\object\FullPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->person_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, $this->person_->size()); // message
        $this->person_->write($fp);
      }
      if (!is_null($this->ancestor_)) {
        fwrite($fp, "\x12");
        \Protobuf::write_varint($fp, $this->ancestor_->size()); // message
        $this->ancestor_->write($fp);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->person_)) {
        $l = $this->person_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->ancestor_)) {
        $l = $this->ancestor_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->person_ === null) return false;
      if ($this->ancestor_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('person_', $this->person_)
           . \Protobuf::toString('ancestor_', $this->ancestor_);
    }
    
    // required .geneweb.api.object.FullPerson person = 1;

    private $person_ = null;
    public function clearPerson() { $this->person_ = null; }
    public function hasPerson() { return $this->person_ !== null; }
    public function getPerson() { if($this->person_ === null) return null; else return $this->person_; }
    public function setPerson(\geneweb\api\object\FullPerson $value) { $this->person_ = $value; }
    
    // required .geneweb.api.object.FullPerson ancestor = 2;

    private $ancestor_ = null;
    public function clearAncestor() { $this->ancestor_ = null; }
    public function hasAncestor() { return $this->ancestor_ !== null; }
    public function getAncestor() { if($this->ancestor_ === null) return null; else return $this->ancestor_; }
    public function setAncestor(\geneweb\api\object\FullPerson $value) { $this->ancestor_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.WarningIncoherentAncestorDate)
  }
  
  // message geneweb.api.object.WarningMarriageDateAfterDeath
  class WarningMarriageDateAfterDeath {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\WarningMarriageDateAfterDeath: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->person_ = new \geneweb\api\object\FullPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->person_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, $this->person_->size()); // message
        $this->person_->write($fp);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->person_)) {
        $l = $this->person_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->person_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('person_', $this->person_);
    }
    
    // required .geneweb.api.object.FullPerson person = 1;

    private $person_ = null;
    public function clearPerson() { $this->person_ = null; }
    public function hasPerson() { return $this->person_ !== null; }
    public function getPerson() { if($this->person_ === null) return null; else return $this->person_; }
    public function setPerson(\geneweb\api\object\FullPerson $value) { $this->person_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.WarningMarriageDateAfterDeath)
  }
  
  // message geneweb.api.object.WarningMarriageDateBeforeBirth
  class WarningMarriageDateBeforeBirth {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\WarningMarriageDateBeforeBirth: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->person_ = new \geneweb\api\object\FullPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->person_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, $this->person_->size()); // message
        $this->person_->write($fp);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->person_)) {
        $l = $this->person_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->person_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('person_', $this->person_);
    }
    
    // required .geneweb.api.object.FullPerson person = 1;

    private $person_ = null;
    public function clearPerson() { $this->person_ = null; }
    public function hasPerson() { return $this->person_ !== null; }
    public function getPerson() { if($this->person_ === null) return null; else return $this->person_; }
    public function setPerson(\geneweb\api\object\FullPerson $value) { $this->person_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.WarningMarriageDateBeforeBirth)
  }
  
  // message geneweb.api.object.WarningMotherDeadBeforeChildBirth
  class WarningMotherDeadBeforeChildBirth {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\WarningMotherDeadBeforeChildBirth: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->mother_ = new \geneweb\api\object\FullPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->child_ = new \geneweb\api\object\FullPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->mother_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, $this->mother_->size()); // message
        $this->mother_->write($fp);
      }
      if (!is_null($this->child_)) {
        fwrite($fp, "\x12");
        \Protobuf::write_varint($fp, $this->child_->size()); // message
        $this->child_->write($fp);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->mother_)) {
        $l = $this->mother_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->child_)) {
        $l = $this->child_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->mother_ === null) return false;
      if ($this->child_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('mother_', $this->mother_)
           . \Protobuf::toString('child_', $this->child_);
    }
    
    // required .geneweb.api.object.FullPerson mother = 1;

    private $mother_ = null;
    public function clearMother() { $this->mother_ = null; }
    public function hasMother() { return $this->mother_ !== null; }
    public function getMother() { if($this->mother_ === null) return null; else return $this->mother_; }
    public function setMother(\geneweb\api\object\FullPerson $value) { $this->mother_ = $value; }
    
    // required .geneweb.api.object.FullPerson child = 2;

    private $child_ = null;
    public function clearChild() { $this->child_ = null; }
    public function hasChild() { return $this->child_ !== null; }
    public function getChild() { if($this->child_ === null) return null; else return $this->child_; }
    public function setChild(\geneweb\api\object\FullPerson $value) { $this->child_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.WarningMotherDeadBeforeChildBirth)
  }
  
  // message geneweb.api.object.WarningParentBornAfterChild
  class WarningParentBornAfterChild {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\WarningParentBornAfterChild: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->parent_ = new \geneweb\api\object\FullPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->child_ = new \geneweb\api\object\FullPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->parent_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, $this->parent_->size()); // message
        $this->parent_->write($fp);
      }
      if (!is_null($this->child_)) {
        fwrite($fp, "\x12");
        \Protobuf::write_varint($fp, $this->child_->size()); // message
        $this->child_->write($fp);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->parent_)) {
        $l = $this->parent_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->child_)) {
        $l = $this->child_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->parent_ === null) return false;
      if ($this->child_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('parent_', $this->parent_)
           . \Protobuf::toString('child_', $this->child_);
    }
    
    // required .geneweb.api.object.FullPerson parent = 1;

    private $parent_ = null;
    public function clearParent() { $this->parent_ = null; }
    public function hasParent() { return $this->parent_ !== null; }
    public function getParent() { if($this->parent_ === null) return null; else return $this->parent_; }
    public function setParent(\geneweb\api\object\FullPerson $value) { $this->parent_ = $value; }
    
    // required .geneweb.api.object.FullPerson child = 2;

    private $child_ = null;
    public function clearChild() { $this->child_ = null; }
    public function hasChild() { return $this->child_ !== null; }
    public function getChild() { if($this->child_ === null) return null; else return $this->child_; }
    public function setChild(\geneweb\api\object\FullPerson $value) { $this->child_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.WarningParentBornAfterChild)
  }
  
  // message geneweb.api.object.WarningParentTooYoung
  class WarningParentTooYoung {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\WarningParentTooYoung: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->parent_ = new \geneweb\api\object\FullPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->date_ = $tmp;
            $limit-=$len;
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->parent_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, $this->parent_->size()); // message
        $this->parent_->write($fp);
      }
      if (!is_null($this->date_)) {
        fwrite($fp, "\x12");
        \Protobuf::write_varint($fp, strlen($this->date_));
        fwrite($fp, $this->date_);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->parent_)) {
        $l = $this->parent_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->date_)) {
        $l = strlen($this->date_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->parent_ === null) return false;
      if ($this->date_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('parent_', $this->parent_)
           . \Protobuf::toString('date_', $this->date_);
    }
    
    // required .geneweb.api.object.FullPerson parent = 1;

    private $parent_ = null;
    public function clearParent() { $this->parent_ = null; }
    public function hasParent() { return $this->parent_ !== null; }
    public function getParent() { if($this->parent_ === null) return null; else return $this->parent_; }
    public function setParent(\geneweb\api\object\FullPerson $value) { $this->parent_ = $value; }
    
    // required string date = 2;

    private $date_ = null;
    public function clearDate() { $this->date_ = null; }
    public function hasDate() { return $this->date_ !== null; }
    public function getDate() { if($this->date_ === null) return ""; else return $this->date_; }
    public function setDate($value) { $this->date_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.WarningParentTooYoung)
  }
  
  // message geneweb.api.object.WarningTitleDatesError
  class WarningTitleDatesError {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\WarningTitleDatesError: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->person_ = new \geneweb\api\object\FullPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->person_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, $this->person_->size()); // message
        $this->person_->write($fp);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->person_)) {
        $l = $this->person_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->person_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('person_', $this->person_);
    }
    
    // required .geneweb.api.object.FullPerson person = 1;

    private $person_ = null;
    public function clearPerson() { $this->person_ = null; }
    public function hasPerson() { return $this->person_ !== null; }
    public function getPerson() { if($this->person_ === null) return null; else return $this->person_; }
    public function setPerson(\geneweb\api\object\FullPerson $value) { $this->person_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.WarningTitleDatesError)
  }
  
  // message geneweb.api.object.WarningUndefinedSex
  class WarningUndefinedSex {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\WarningUndefinedSex: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->person_ = new \geneweb\api\object\FullPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->person_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, $this->person_->size()); // message
        $this->person_->write($fp);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->person_)) {
        $l = $this->person_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->person_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('person_', $this->person_);
    }
    
    // required .geneweb.api.object.FullPerson person = 1;

    private $person_ = null;
    public function clearPerson() { $this->person_ = null; }
    public function hasPerson() { return $this->person_ !== null; }
    public function getPerson() { if($this->person_ === null) return null; else return $this->person_; }
    public function setPerson(\geneweb\api\object\FullPerson $value) { $this->person_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.WarningUndefinedSex)
  }
  
  // message geneweb.api.object.WarningYoungForMarriage
  class WarningYoungForMarriage {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\WarningYoungForMarriage: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->person_ = new \geneweb\api\object\FullPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->date_ = $tmp;
            $limit-=$len;
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->person_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, $this->person_->size()); // message
        $this->person_->write($fp);
      }
      if (!is_null($this->date_)) {
        fwrite($fp, "\x12");
        \Protobuf::write_varint($fp, strlen($this->date_));
        fwrite($fp, $this->date_);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->person_)) {
        $l = $this->person_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->date_)) {
        $l = strlen($this->date_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->person_ === null) return false;
      if ($this->date_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('person_', $this->person_)
           . \Protobuf::toString('date_', $this->date_);
    }
    
    // required .geneweb.api.object.FullPerson person = 1;

    private $person_ = null;
    public function clearPerson() { $this->person_ = null; }
    public function hasPerson() { return $this->person_ !== null; }
    public function getPerson() { if($this->person_ === null) return null; else return $this->person_; }
    public function setPerson(\geneweb\api\object\FullPerson $value) { $this->person_ = $value; }
    
    // required string date = 2;

    private $date_ = null;
    public function clearDate() { $this->date_ = null; }
    public function hasDate() { return $this->date_ !== null; }
    public function getDate() { if($this->date_ === null) return ""; else return $this->date_; }
    public function setDate($value) { $this->date_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.WarningYoungForMarriage)
  }
  
  // message geneweb.api.object.WarningParentTooOld
  class WarningParentTooOld {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\WarningParentTooOld: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->parent_ = new \geneweb\api\object\FullPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->date_ = $tmp;
            $limit-=$len;
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->parent_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, $this->parent_->size()); // message
        $this->parent_->write($fp);
      }
      if (!is_null($this->date_)) {
        fwrite($fp, "\x12");
        \Protobuf::write_varint($fp, strlen($this->date_));
        fwrite($fp, $this->date_);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->parent_)) {
        $l = $this->parent_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->date_)) {
        $l = strlen($this->date_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->parent_ === null) return false;
      if ($this->date_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('parent_', $this->parent_)
           . \Protobuf::toString('date_', $this->date_);
    }
    
    // required .geneweb.api.object.FullPerson parent = 1;

    private $parent_ = null;
    public function clearParent() { $this->parent_ = null; }
    public function hasParent() { return $this->parent_ !== null; }
    public function getParent() { if($this->parent_ === null) return null; else return $this->parent_; }
    public function setParent(\geneweb\api\object\FullPerson $value) { $this->parent_ = $value; }
    
    // required string date = 2;

    private $date_ = null;
    public function clearDate() { $this->date_ = null; }
    public function hasDate() { return $this->date_ !== null; }
    public function getDate() { if($this->date_ === null) return ""; else return $this->date_; }
    public function setDate($value) { $this->date_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.WarningParentTooOld)
  }
  
  // message geneweb.api.object.WarningCloseChildren
  class WarningCloseChildren {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\WarningCloseChildren: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->father_ = new \geneweb\api\object\FullPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->mother_ = new \geneweb\api\object\FullPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          case 3:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->child1_ = new \geneweb\api\object\FullPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          case 4:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->child2_ = new \geneweb\api\object\FullPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->father_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, $this->father_->size()); // message
        $this->father_->write($fp);
      }
      if (!is_null($this->mother_)) {
        fwrite($fp, "\x12");
        \Protobuf::write_varint($fp, $this->mother_->size()); // message
        $this->mother_->write($fp);
      }
      if (!is_null($this->child1_)) {
        fwrite($fp, "\x1a");
        \Protobuf::write_varint($fp, $this->child1_->size()); // message
        $this->child1_->write($fp);
      }
      if (!is_null($this->child2_)) {
        fwrite($fp, "\"");
        \Protobuf::write_varint($fp, $this->child2_->size()); // message
        $this->child2_->write($fp);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->father_)) {
        $l = $this->father_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->mother_)) {
        $l = $this->mother_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->child1_)) {
        $l = $this->child1_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->child2_)) {
        $l = $this->child2_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->father_ === null) return false;
      if ($this->mother_ === null) return false;
      if ($this->child1_ === null) return false;
      if ($this->child2_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('father_', $this->father_)
           . \Protobuf::toString('mother_', $this->mother_)
           . \Protobuf::toString('child1_', $this->child1_)
           . \Protobuf::toString('child2_', $this->child2_);
    }
    
    // required .geneweb.api.object.FullPerson father = 1;

    private $father_ = null;
    public function clearFather() { $this->father_ = null; }
    public function hasFather() { return $this->father_ !== null; }
    public function getFather() { if($this->father_ === null) return null; else return $this->father_; }
    public function setFather(\geneweb\api\object\FullPerson $value) { $this->father_ = $value; }
    
    // required .geneweb.api.object.FullPerson mother = 2;

    private $mother_ = null;
    public function clearMother() { $this->mother_ = null; }
    public function hasMother() { return $this->mother_ !== null; }
    public function getMother() { if($this->mother_ === null) return null; else return $this->mother_; }
    public function setMother(\geneweb\api\object\FullPerson $value) { $this->mother_ = $value; }
    
    // required .geneweb.api.object.FullPerson child1 = 3;

    private $child1_ = null;
    public function clearChild1() { $this->child1_ = null; }
    public function hasChild1() { return $this->child1_ !== null; }
    public function getChild1() { if($this->child1_ === null) return null; else return $this->child1_; }
    public function setChild1(\geneweb\api\object\FullPerson $value) { $this->child1_ = $value; }
    
    // required .geneweb.api.object.FullPerson child2 = 4;

    private $child2_ = null;
    public function clearChild2() { $this->child2_ = null; }
    public function hasChild2() { return $this->child2_ !== null; }
    public function getChild2() { if($this->child2_ === null) return null; else return $this->child2_; }
    public function setChild2(\geneweb\api\object\FullPerson $value) { $this->child2_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.WarningCloseChildren)
  }
  
  // message geneweb.api.object.WarningBigAgeBetweenSpouses
  class WarningBigAgeBetweenSpouses {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\WarningBigAgeBetweenSpouses: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->father_ = new \geneweb\api\object\FullPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->mother_ = new \geneweb\api\object\FullPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          case 3:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->date_ = $tmp;
            $limit-=$len;
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->father_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, $this->father_->size()); // message
        $this->father_->write($fp);
      }
      if (!is_null($this->mother_)) {
        fwrite($fp, "\x12");
        \Protobuf::write_varint($fp, $this->mother_->size()); // message
        $this->mother_->write($fp);
      }
      if (!is_null($this->date_)) {
        fwrite($fp, "\x1a");
        \Protobuf::write_varint($fp, strlen($this->date_));
        fwrite($fp, $this->date_);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->father_)) {
        $l = $this->father_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->mother_)) {
        $l = $this->mother_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->date_)) {
        $l = strlen($this->date_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->father_ === null) return false;
      if ($this->mother_ === null) return false;
      if ($this->date_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('father_', $this->father_)
           . \Protobuf::toString('mother_', $this->mother_)
           . \Protobuf::toString('date_', $this->date_);
    }
    
    // required .geneweb.api.object.FullPerson father = 1;

    private $father_ = null;
    public function clearFather() { $this->father_ = null; }
    public function hasFather() { return $this->father_ !== null; }
    public function getFather() { if($this->father_ === null) return null; else return $this->father_; }
    public function setFather(\geneweb\api\object\FullPerson $value) { $this->father_ = $value; }
    
    // required .geneweb.api.object.FullPerson mother = 2;

    private $mother_ = null;
    public function clearMother() { $this->mother_ = null; }
    public function hasMother() { return $this->mother_ !== null; }
    public function getMother() { if($this->mother_ === null) return null; else return $this->mother_; }
    public function setMother(\geneweb\api\object\FullPerson $value) { $this->mother_ = $value; }
    
    // required string date = 3;

    private $date_ = null;
    public function clearDate() { $this->date_ = null; }
    public function hasDate() { return $this->date_ !== null; }
    public function getDate() { if($this->date_ === null) return ""; else return $this->date_; }
    public function setDate($value) { $this->date_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.WarningBigAgeBetweenSpouses)
  }
  
  // message geneweb.api.object.WarningDeadOld
  class WarningDeadOld {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\WarningDeadOld: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->person_ = new \geneweb\api\object\FullPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          case 3:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->date_ = $tmp;
            $limit-=$len;
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->person_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, $this->person_->size()); // message
        $this->person_->write($fp);
      }
      if (!is_null($this->date_)) {
        fwrite($fp, "\x1a");
        \Protobuf::write_varint($fp, strlen($this->date_));
        fwrite($fp, $this->date_);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->person_)) {
        $l = $this->person_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->date_)) {
        $l = strlen($this->date_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->person_ === null) return false;
      if ($this->date_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('person_', $this->person_)
           . \Protobuf::toString('date_', $this->date_);
    }
    
    // required .geneweb.api.object.FullPerson person = 1;

    private $person_ = null;
    public function clearPerson() { $this->person_ = null; }
    public function hasPerson() { return $this->person_ !== null; }
    public function getPerson() { if($this->person_ === null) return null; else return $this->person_; }
    public function setPerson(\geneweb\api\object\FullPerson $value) { $this->person_ = $value; }
    
    // required string date = 3;

    private $date_ = null;
    public function clearDate() { $this->date_ = null; }
    public function hasDate() { return $this->date_ !== null; }
    public function getDate() { if($this->date_ === null) return ""; else return $this->date_; }
    public function setDate($value) { $this->date_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.WarningDeadOld)
  }
  
  // message geneweb.api.object.WarningOldIndividual
  class WarningOldIndividual {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\WarningOldIndividual: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->person_ = new \geneweb\api\object\FullPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          case 3:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->date_ = $tmp;
            $limit-=$len;
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->person_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, $this->person_->size()); // message
        $this->person_->write($fp);
      }
      if (!is_null($this->date_)) {
        fwrite($fp, "\x1a");
        \Protobuf::write_varint($fp, strlen($this->date_));
        fwrite($fp, $this->date_);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->person_)) {
        $l = $this->person_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->date_)) {
        $l = strlen($this->date_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->person_ === null) return false;
      if ($this->date_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('person_', $this->person_)
           . \Protobuf::toString('date_', $this->date_);
    }
    
    // required .geneweb.api.object.FullPerson person = 1;

    private $person_ = null;
    public function clearPerson() { $this->person_ = null; }
    public function hasPerson() { return $this->person_ !== null; }
    public function getPerson() { if($this->person_ === null) return null; else return $this->person_; }
    public function setPerson(\geneweb\api\object\FullPerson $value) { $this->person_ = $value; }
    
    // required string date = 3;

    private $date_ = null;
    public function clearDate() { $this->date_ = null; }
    public function hasDate() { return $this->date_ !== null; }
    public function getDate() { if($this->date_ === null) return ""; else return $this->date_; }
    public function setDate($value) { $this->date_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.WarningOldIndividual)
  }
  
  // message geneweb.api.object.WarningWitnessDateAfterDeath
  class WarningWitnessDateAfterDeath {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\WarningWitnessDateAfterDeath: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->person_ = new \geneweb\api\object\FullPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->person_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, $this->person_->size()); // message
        $this->person_->write($fp);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->person_)) {
        $l = $this->person_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->person_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('person_', $this->person_);
    }
    
    // required .geneweb.api.object.FullPerson person = 1;

    private $person_ = null;
    public function clearPerson() { $this->person_ = null; }
    public function hasPerson() { return $this->person_ !== null; }
    public function getPerson() { if($this->person_ === null) return null; else return $this->person_; }
    public function setPerson(\geneweb\api\object\FullPerson $value) { $this->person_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.WarningWitnessDateAfterDeath)
  }
  
  // message geneweb.api.object.WarningWitnessDateBeforeBirth
  class WarningWitnessDateBeforeBirth {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\WarningWitnessDateBeforeBirth: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->person_ = new \geneweb\api\object\FullPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->person_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, $this->person_->size()); // message
        $this->person_->write($fp);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->person_)) {
        $l = $this->person_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->person_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('person_', $this->person_);
    }
    
    // required .geneweb.api.object.FullPerson person = 1;

    private $person_ = null;
    public function clearPerson() { $this->person_ = null; }
    public function hasPerson() { return $this->person_ !== null; }
    public function getPerson() { if($this->person_ === null) return null; else return $this->person_; }
    public function setPerson(\geneweb\api\object\FullPerson $value) { $this->person_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.WarningWitnessDateBeforeBirth)
  }
  
  // message geneweb.api.object.BaseWarnings
  class BaseWarnings {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\BaseWarnings: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->warningAlreadyDefined_[] = new \geneweb\api\object\WarningAlreadyDefined($fp, $len);
            ASSERT('$len == 0');
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->warningOwnAncestor_[] = new \geneweb\api\object\WarningOwnAncestor($fp, $len);
            ASSERT('$len == 0');
            break;
          case 3:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->warningBadSexOfMarriedPerson_[] = new \geneweb\api\object\WarningBadSexOfMarriedPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          case 4:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->warningBirthAfterDeath_[] = new \geneweb\api\object\WarningBirthAfterDeath($fp, $len);
            ASSERT('$len == 0');
            break;
          case 5:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->warningIncoherentSex_[] = new \geneweb\api\object\WarningIncoherentSex($fp, $len);
            ASSERT('$len == 0');
            break;
          case 6:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->warningChangedOrderOfChildren_[] = new \geneweb\api\object\WarningChangedOrderOfChildren($fp, $len);
            ASSERT('$len == 0');
            break;
          case 7:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->warningChildrenNotInOrder_[] = new \geneweb\api\object\WarningChildrenNotInOrder($fp, $len);
            ASSERT('$len == 0');
            break;
          case 8:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->warningDeadTooEarlyToBeFather_[] = new \geneweb\api\object\WarningDeadTooEarlyToBeFather($fp, $len);
            ASSERT('$len == 0');
            break;
          case 9:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->warningIncoherentAncestorDate_[] = new \geneweb\api\object\WarningIncoherentAncestorDate($fp, $len);
            ASSERT('$len == 0');
            break;
          case 10:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->warningMarriageDateAfterDeath_[] = new \geneweb\api\object\WarningMarriageDateAfterDeath($fp, $len);
            ASSERT('$len == 0');
            break;
          case 11:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->warningMarriageDateBeforeBirth_[] = new \geneweb\api\object\WarningMarriageDateBeforeBirth($fp, $len);
            ASSERT('$len == 0');
            break;
          case 12:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->warningMotherDeadBeforeChildBirth_[] = new \geneweb\api\object\WarningMotherDeadBeforeChildBirth($fp, $len);
            ASSERT('$len == 0');
            break;
          case 13:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->warningParentBornAfterChild_[] = new \geneweb\api\object\WarningParentBornAfterChild($fp, $len);
            ASSERT('$len == 0');
            break;
          case 14:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->warningParentTooYoung_[] = new \geneweb\api\object\WarningParentTooYoung($fp, $len);
            ASSERT('$len == 0');
            break;
          case 15:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->warningTitleDatesError_[] = new \geneweb\api\object\WarningTitleDatesError($fp, $len);
            ASSERT('$len == 0');
            break;
          case 16:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->warningUndefinedSex_[] = new \geneweb\api\object\WarningUndefinedSex($fp, $len);
            ASSERT('$len == 0');
            break;
          case 17:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->warningYoungForMarriage_[] = new \geneweb\api\object\WarningYoungForMarriage($fp, $len);
            ASSERT('$len == 0');
            break;
          case 18:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->warningCloseChildren_[] = new \geneweb\api\object\WarningCloseChildren($fp, $len);
            ASSERT('$len == 0');
            break;
          case 19:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->warningParentTooOld_[] = new \geneweb\api\object\WarningParentTooOld($fp, $len);
            ASSERT('$len == 0');
            break;
          case 20:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->warningChangedOrderOfMarriages_[] = new \geneweb\api\object\WarningChangedOrderOfMarriages($fp, $len);
            ASSERT('$len == 0');
            break;
          case 21:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->warningBigAgeBetweenSpouses_[] = new \geneweb\api\object\WarningBigAgeBetweenSpouses($fp, $len);
            ASSERT('$len == 0');
            break;
          case 22:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->warningDeadOld_[] = new \geneweb\api\object\WarningDeadOld($fp, $len);
            ASSERT('$len == 0');
            break;
          case 23:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->warningOldIndividual_[] = new \geneweb\api\object\WarningOldIndividual($fp, $len);
            ASSERT('$len == 0');
            break;
          case 24:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->warningWitnessDateAfterDeath_[] = new \geneweb\api\object\WarningWitnessDateAfterDeath($fp, $len);
            ASSERT('$len == 0');
            break;
          case 25:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->warningWitnessDateBeforeBirth_[] = new \geneweb\api\object\WarningWitnessDateBeforeBirth($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->warningAlreadyDefined_))
        foreach($this->warningAlreadyDefined_ as $v) {
          fwrite($fp, "\x0a");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->warningOwnAncestor_))
        foreach($this->warningOwnAncestor_ as $v) {
          fwrite($fp, "\x12");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->warningBadSexOfMarriedPerson_))
        foreach($this->warningBadSexOfMarriedPerson_ as $v) {
          fwrite($fp, "\x1a");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->warningBirthAfterDeath_))
        foreach($this->warningBirthAfterDeath_ as $v) {
          fwrite($fp, "\"");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->warningIncoherentSex_))
        foreach($this->warningIncoherentSex_ as $v) {
          fwrite($fp, "*");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->warningChangedOrderOfChildren_))
        foreach($this->warningChangedOrderOfChildren_ as $v) {
          fwrite($fp, "2");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->warningChildrenNotInOrder_))
        foreach($this->warningChildrenNotInOrder_ as $v) {
          fwrite($fp, ":");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->warningDeadTooEarlyToBeFather_))
        foreach($this->warningDeadTooEarlyToBeFather_ as $v) {
          fwrite($fp, "B");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->warningIncoherentAncestorDate_))
        foreach($this->warningIncoherentAncestorDate_ as $v) {
          fwrite($fp, "J");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->warningMarriageDateAfterDeath_))
        foreach($this->warningMarriageDateAfterDeath_ as $v) {
          fwrite($fp, "R");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->warningMarriageDateBeforeBirth_))
        foreach($this->warningMarriageDateBeforeBirth_ as $v) {
          fwrite($fp, "Z");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->warningMotherDeadBeforeChildBirth_))
        foreach($this->warningMotherDeadBeforeChildBirth_ as $v) {
          fwrite($fp, "b");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->warningParentBornAfterChild_))
        foreach($this->warningParentBornAfterChild_ as $v) {
          fwrite($fp, "j");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->warningParentTooYoung_))
        foreach($this->warningParentTooYoung_ as $v) {
          fwrite($fp, "r");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->warningTitleDatesError_))
        foreach($this->warningTitleDatesError_ as $v) {
          fwrite($fp, "z");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->warningUndefinedSex_))
        foreach($this->warningUndefinedSex_ as $v) {
          fwrite($fp, "\x82\x01");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->warningYoungForMarriage_))
        foreach($this->warningYoungForMarriage_ as $v) {
          fwrite($fp, "\x8a\x01");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->warningCloseChildren_))
        foreach($this->warningCloseChildren_ as $v) {
          fwrite($fp, "\x92\x01");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->warningParentTooOld_))
        foreach($this->warningParentTooOld_ as $v) {
          fwrite($fp, "\x9a\x01");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->warningChangedOrderOfMarriages_))
        foreach($this->warningChangedOrderOfMarriages_ as $v) {
          fwrite($fp, "\xa2\x01");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->warningBigAgeBetweenSpouses_))
        foreach($this->warningBigAgeBetweenSpouses_ as $v) {
          fwrite($fp, "\xaa\x01");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->warningDeadOld_))
        foreach($this->warningDeadOld_ as $v) {
          fwrite($fp, "\xb2\x01");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->warningOldIndividual_))
        foreach($this->warningOldIndividual_ as $v) {
          fwrite($fp, "\xba\x01");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->warningWitnessDateAfterDeath_))
        foreach($this->warningWitnessDateAfterDeath_ as $v) {
          fwrite($fp, "\xc2\x01");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->warningWitnessDateBeforeBirth_))
        foreach($this->warningWitnessDateBeforeBirth_ as $v) {
          fwrite($fp, "\xca\x01");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->warningAlreadyDefined_))
        foreach($this->warningAlreadyDefined_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->warningOwnAncestor_))
        foreach($this->warningOwnAncestor_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->warningBadSexOfMarriedPerson_))
        foreach($this->warningBadSexOfMarriedPerson_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->warningBirthAfterDeath_))
        foreach($this->warningBirthAfterDeath_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->warningIncoherentSex_))
        foreach($this->warningIncoherentSex_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->warningChangedOrderOfChildren_))
        foreach($this->warningChangedOrderOfChildren_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->warningChildrenNotInOrder_))
        foreach($this->warningChildrenNotInOrder_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->warningDeadTooEarlyToBeFather_))
        foreach($this->warningDeadTooEarlyToBeFather_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->warningIncoherentAncestorDate_))
        foreach($this->warningIncoherentAncestorDate_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->warningMarriageDateAfterDeath_))
        foreach($this->warningMarriageDateAfterDeath_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->warningMarriageDateBeforeBirth_))
        foreach($this->warningMarriageDateBeforeBirth_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->warningMotherDeadBeforeChildBirth_))
        foreach($this->warningMotherDeadBeforeChildBirth_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->warningParentBornAfterChild_))
        foreach($this->warningParentBornAfterChild_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->warningParentTooYoung_))
        foreach($this->warningParentTooYoung_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->warningTitleDatesError_))
        foreach($this->warningTitleDatesError_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->warningUndefinedSex_))
        foreach($this->warningUndefinedSex_ as $v) {
          $l = $v->size();
          $size += 2 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->warningYoungForMarriage_))
        foreach($this->warningYoungForMarriage_ as $v) {
          $l = $v->size();
          $size += 2 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->warningCloseChildren_))
        foreach($this->warningCloseChildren_ as $v) {
          $l = $v->size();
          $size += 2 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->warningParentTooOld_))
        foreach($this->warningParentTooOld_ as $v) {
          $l = $v->size();
          $size += 2 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->warningChangedOrderOfMarriages_))
        foreach($this->warningChangedOrderOfMarriages_ as $v) {
          $l = $v->size();
          $size += 2 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->warningBigAgeBetweenSpouses_))
        foreach($this->warningBigAgeBetweenSpouses_ as $v) {
          $l = $v->size();
          $size += 2 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->warningDeadOld_))
        foreach($this->warningDeadOld_ as $v) {
          $l = $v->size();
          $size += 2 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->warningOldIndividual_))
        foreach($this->warningOldIndividual_ as $v) {
          $l = $v->size();
          $size += 2 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->warningWitnessDateAfterDeath_))
        foreach($this->warningWitnessDateAfterDeath_ as $v) {
          $l = $v->size();
          $size += 2 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->warningWitnessDateBeforeBirth_))
        foreach($this->warningWitnessDateBeforeBirth_ as $v) {
          $l = $v->size();
          $size += 2 + \Protobuf::size_varint($l) + $l;
        }
      return $size;
    }
    
    public function validateRequired() {
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('warningAlreadyDefined_', $this->warningAlreadyDefined_)
           . \Protobuf::toString('warningOwnAncestor_', $this->warningOwnAncestor_)
           . \Protobuf::toString('warningBadSexOfMarriedPerson_', $this->warningBadSexOfMarriedPerson_)
           . \Protobuf::toString('warningBirthAfterDeath_', $this->warningBirthAfterDeath_)
           . \Protobuf::toString('warningIncoherentSex_', $this->warningIncoherentSex_)
           . \Protobuf::toString('warningChangedOrderOfChildren_', $this->warningChangedOrderOfChildren_)
           . \Protobuf::toString('warningChildrenNotInOrder_', $this->warningChildrenNotInOrder_)
           . \Protobuf::toString('warningDeadTooEarlyToBeFather_', $this->warningDeadTooEarlyToBeFather_)
           . \Protobuf::toString('warningIncoherentAncestorDate_', $this->warningIncoherentAncestorDate_)
           . \Protobuf::toString('warningMarriageDateAfterDeath_', $this->warningMarriageDateAfterDeath_)
           . \Protobuf::toString('warningMarriageDateBeforeBirth_', $this->warningMarriageDateBeforeBirth_)
           . \Protobuf::toString('warningMotherDeadBeforeChildBirth_', $this->warningMotherDeadBeforeChildBirth_)
           . \Protobuf::toString('warningParentBornAfterChild_', $this->warningParentBornAfterChild_)
           . \Protobuf::toString('warningParentTooYoung_', $this->warningParentTooYoung_)
           . \Protobuf::toString('warningTitleDatesError_', $this->warningTitleDatesError_)
           . \Protobuf::toString('warningUndefinedSex_', $this->warningUndefinedSex_)
           . \Protobuf::toString('warningYoungForMarriage_', $this->warningYoungForMarriage_)
           . \Protobuf::toString('warningCloseChildren_', $this->warningCloseChildren_)
           . \Protobuf::toString('warningParentTooOld_', $this->warningParentTooOld_)
           . \Protobuf::toString('warningChangedOrderOfMarriages_', $this->warningChangedOrderOfMarriages_)
           . \Protobuf::toString('warningBigAgeBetweenSpouses_', $this->warningBigAgeBetweenSpouses_)
           . \Protobuf::toString('warningDeadOld_', $this->warningDeadOld_)
           . \Protobuf::toString('warningOldIndividual_', $this->warningOldIndividual_)
           . \Protobuf::toString('warningWitnessDateAfterDeath_', $this->warningWitnessDateAfterDeath_)
           . \Protobuf::toString('warningWitnessDateBeforeBirth_', $this->warningWitnessDateBeforeBirth_);
    }
    
    // repeated .geneweb.api.object.WarningAlreadyDefined warning_already_defined = 1;

    private $warningAlreadyDefined_ = null;
    public function clearWarningAlreadyDefined() { $this->warningAlreadyDefined_ = null; }
    public function getWarningAlreadyDefinedCount() { if ($this->warningAlreadyDefined_ === null ) return 0; else return count($this->warningAlreadyDefined_); }
    public function getWarningAlreadyDefined($index) { return $this->warningAlreadyDefined_[$index]; }
    public function getWarningAlreadyDefinedArray() { if ($this->warningAlreadyDefined_ === null ) return array(); else return $this->warningAlreadyDefined_; }
    public function setWarningAlreadyDefined($index, $value) {$this->warningAlreadyDefined_[$index] = $value;	}
    public function addWarningAlreadyDefined($value) { $this->warningAlreadyDefined_[] = $value; }
    public function addAllWarningAlreadyDefined(array $values) { foreach($values as $value) {$this->warningAlreadyDefined_[] = $value;} }
    
    // repeated .geneweb.api.object.WarningOwnAncestor warning_own_ancestor = 2;

    private $warningOwnAncestor_ = null;
    public function clearWarningOwnAncestor() { $this->warningOwnAncestor_ = null; }
    public function getWarningOwnAncestorCount() { if ($this->warningOwnAncestor_ === null ) return 0; else return count($this->warningOwnAncestor_); }
    public function getWarningOwnAncestor($index) { return $this->warningOwnAncestor_[$index]; }
    public function getWarningOwnAncestorArray() { if ($this->warningOwnAncestor_ === null ) return array(); else return $this->warningOwnAncestor_; }
    public function setWarningOwnAncestor($index, $value) {$this->warningOwnAncestor_[$index] = $value;	}
    public function addWarningOwnAncestor($value) { $this->warningOwnAncestor_[] = $value; }
    public function addAllWarningOwnAncestor(array $values) { foreach($values as $value) {$this->warningOwnAncestor_[] = $value;} }
    
    // repeated .geneweb.api.object.WarningBadSexOfMarriedPerson warning_bad_sex_of_married_person = 3;

    private $warningBadSexOfMarriedPerson_ = null;
    public function clearWarningBadSexOfMarriedPerson() { $this->warningBadSexOfMarriedPerson_ = null; }
    public function getWarningBadSexOfMarriedPersonCount() { if ($this->warningBadSexOfMarriedPerson_ === null ) return 0; else return count($this->warningBadSexOfMarriedPerson_); }
    public function getWarningBadSexOfMarriedPerson($index) { return $this->warningBadSexOfMarriedPerson_[$index]; }
    public function getWarningBadSexOfMarriedPersonArray() { if ($this->warningBadSexOfMarriedPerson_ === null ) return array(); else return $this->warningBadSexOfMarriedPerson_; }
    public function setWarningBadSexOfMarriedPerson($index, $value) {$this->warningBadSexOfMarriedPerson_[$index] = $value;	}
    public function addWarningBadSexOfMarriedPerson($value) { $this->warningBadSexOfMarriedPerson_[] = $value; }
    public function addAllWarningBadSexOfMarriedPerson(array $values) { foreach($values as $value) {$this->warningBadSexOfMarriedPerson_[] = $value;} }
    
    // repeated .geneweb.api.object.WarningBirthAfterDeath warning_birth_after_death = 4;

    private $warningBirthAfterDeath_ = null;
    public function clearWarningBirthAfterDeath() { $this->warningBirthAfterDeath_ = null; }
    public function getWarningBirthAfterDeathCount() { if ($this->warningBirthAfterDeath_ === null ) return 0; else return count($this->warningBirthAfterDeath_); }
    public function getWarningBirthAfterDeath($index) { return $this->warningBirthAfterDeath_[$index]; }
    public function getWarningBirthAfterDeathArray() { if ($this->warningBirthAfterDeath_ === null ) return array(); else return $this->warningBirthAfterDeath_; }
    public function setWarningBirthAfterDeath($index, $value) {$this->warningBirthAfterDeath_[$index] = $value;	}
    public function addWarningBirthAfterDeath($value) { $this->warningBirthAfterDeath_[] = $value; }
    public function addAllWarningBirthAfterDeath(array $values) { foreach($values as $value) {$this->warningBirthAfterDeath_[] = $value;} }
    
    // repeated .geneweb.api.object.WarningIncoherentSex warning_incoherent_sex = 5;

    private $warningIncoherentSex_ = null;
    public function clearWarningIncoherentSex() { $this->warningIncoherentSex_ = null; }
    public function getWarningIncoherentSexCount() { if ($this->warningIncoherentSex_ === null ) return 0; else return count($this->warningIncoherentSex_); }
    public function getWarningIncoherentSex($index) { return $this->warningIncoherentSex_[$index]; }
    public function getWarningIncoherentSexArray() { if ($this->warningIncoherentSex_ === null ) return array(); else return $this->warningIncoherentSex_; }
    public function setWarningIncoherentSex($index, $value) {$this->warningIncoherentSex_[$index] = $value;	}
    public function addWarningIncoherentSex($value) { $this->warningIncoherentSex_[] = $value; }
    public function addAllWarningIncoherentSex(array $values) { foreach($values as $value) {$this->warningIncoherentSex_[] = $value;} }
    
    // repeated .geneweb.api.object.WarningChangedOrderOfChildren warning_changed_order_of_children = 6;

    private $warningChangedOrderOfChildren_ = null;
    public function clearWarningChangedOrderOfChildren() { $this->warningChangedOrderOfChildren_ = null; }
    public function getWarningChangedOrderOfChildrenCount() { if ($this->warningChangedOrderOfChildren_ === null ) return 0; else return count($this->warningChangedOrderOfChildren_); }
    public function getWarningChangedOrderOfChildren($index) { return $this->warningChangedOrderOfChildren_[$index]; }
    public function getWarningChangedOrderOfChildrenArray() { if ($this->warningChangedOrderOfChildren_ === null ) return array(); else return $this->warningChangedOrderOfChildren_; }
    public function setWarningChangedOrderOfChildren($index, $value) {$this->warningChangedOrderOfChildren_[$index] = $value;	}
    public function addWarningChangedOrderOfChildren($value) { $this->warningChangedOrderOfChildren_[] = $value; }
    public function addAllWarningChangedOrderOfChildren(array $values) { foreach($values as $value) {$this->warningChangedOrderOfChildren_[] = $value;} }
    
    // repeated .geneweb.api.object.WarningChildrenNotInOrder warning_children_not_in_order = 7;

    private $warningChildrenNotInOrder_ = null;
    public function clearWarningChildrenNotInOrder() { $this->warningChildrenNotInOrder_ = null; }
    public function getWarningChildrenNotInOrderCount() { if ($this->warningChildrenNotInOrder_ === null ) return 0; else return count($this->warningChildrenNotInOrder_); }
    public function getWarningChildrenNotInOrder($index) { return $this->warningChildrenNotInOrder_[$index]; }
    public function getWarningChildrenNotInOrderArray() { if ($this->warningChildrenNotInOrder_ === null ) return array(); else return $this->warningChildrenNotInOrder_; }
    public function setWarningChildrenNotInOrder($index, $value) {$this->warningChildrenNotInOrder_[$index] = $value;	}
    public function addWarningChildrenNotInOrder($value) { $this->warningChildrenNotInOrder_[] = $value; }
    public function addAllWarningChildrenNotInOrder(array $values) { foreach($values as $value) {$this->warningChildrenNotInOrder_[] = $value;} }
    
    // repeated .geneweb.api.object.WarningDeadTooEarlyToBeFather warning_dead_too_early_to_be_father = 8;

    private $warningDeadTooEarlyToBeFather_ = null;
    public function clearWarningDeadTooEarlyToBeFather() { $this->warningDeadTooEarlyToBeFather_ = null; }
    public function getWarningDeadTooEarlyToBeFatherCount() { if ($this->warningDeadTooEarlyToBeFather_ === null ) return 0; else return count($this->warningDeadTooEarlyToBeFather_); }
    public function getWarningDeadTooEarlyToBeFather($index) { return $this->warningDeadTooEarlyToBeFather_[$index]; }
    public function getWarningDeadTooEarlyToBeFatherArray() { if ($this->warningDeadTooEarlyToBeFather_ === null ) return array(); else return $this->warningDeadTooEarlyToBeFather_; }
    public function setWarningDeadTooEarlyToBeFather($index, $value) {$this->warningDeadTooEarlyToBeFather_[$index] = $value;	}
    public function addWarningDeadTooEarlyToBeFather($value) { $this->warningDeadTooEarlyToBeFather_[] = $value; }
    public function addAllWarningDeadTooEarlyToBeFather(array $values) { foreach($values as $value) {$this->warningDeadTooEarlyToBeFather_[] = $value;} }
    
    // repeated .geneweb.api.object.WarningIncoherentAncestorDate warning_incoherent_ancestor_date = 9;

    private $warningIncoherentAncestorDate_ = null;
    public function clearWarningIncoherentAncestorDate() { $this->warningIncoherentAncestorDate_ = null; }
    public function getWarningIncoherentAncestorDateCount() { if ($this->warningIncoherentAncestorDate_ === null ) return 0; else return count($this->warningIncoherentAncestorDate_); }
    public function getWarningIncoherentAncestorDate($index) { return $this->warningIncoherentAncestorDate_[$index]; }
    public function getWarningIncoherentAncestorDateArray() { if ($this->warningIncoherentAncestorDate_ === null ) return array(); else return $this->warningIncoherentAncestorDate_; }
    public function setWarningIncoherentAncestorDate($index, $value) {$this->warningIncoherentAncestorDate_[$index] = $value;	}
    public function addWarningIncoherentAncestorDate($value) { $this->warningIncoherentAncestorDate_[] = $value; }
    public function addAllWarningIncoherentAncestorDate(array $values) { foreach($values as $value) {$this->warningIncoherentAncestorDate_[] = $value;} }
    
    // repeated .geneweb.api.object.WarningMarriageDateAfterDeath warning_marriage_date_after_death = 10;

    private $warningMarriageDateAfterDeath_ = null;
    public function clearWarningMarriageDateAfterDeath() { $this->warningMarriageDateAfterDeath_ = null; }
    public function getWarningMarriageDateAfterDeathCount() { if ($this->warningMarriageDateAfterDeath_ === null ) return 0; else return count($this->warningMarriageDateAfterDeath_); }
    public function getWarningMarriageDateAfterDeath($index) { return $this->warningMarriageDateAfterDeath_[$index]; }
    public function getWarningMarriageDateAfterDeathArray() { if ($this->warningMarriageDateAfterDeath_ === null ) return array(); else return $this->warningMarriageDateAfterDeath_; }
    public function setWarningMarriageDateAfterDeath($index, $value) {$this->warningMarriageDateAfterDeath_[$index] = $value;	}
    public function addWarningMarriageDateAfterDeath($value) { $this->warningMarriageDateAfterDeath_[] = $value; }
    public function addAllWarningMarriageDateAfterDeath(array $values) { foreach($values as $value) {$this->warningMarriageDateAfterDeath_[] = $value;} }
    
    // repeated .geneweb.api.object.WarningMarriageDateBeforeBirth warning_marriage_date_before_birth = 11;

    private $warningMarriageDateBeforeBirth_ = null;
    public function clearWarningMarriageDateBeforeBirth() { $this->warningMarriageDateBeforeBirth_ = null; }
    public function getWarningMarriageDateBeforeBirthCount() { if ($this->warningMarriageDateBeforeBirth_ === null ) return 0; else return count($this->warningMarriageDateBeforeBirth_); }
    public function getWarningMarriageDateBeforeBirth($index) { return $this->warningMarriageDateBeforeBirth_[$index]; }
    public function getWarningMarriageDateBeforeBirthArray() { if ($this->warningMarriageDateBeforeBirth_ === null ) return array(); else return $this->warningMarriageDateBeforeBirth_; }
    public function setWarningMarriageDateBeforeBirth($index, $value) {$this->warningMarriageDateBeforeBirth_[$index] = $value;	}
    public function addWarningMarriageDateBeforeBirth($value) { $this->warningMarriageDateBeforeBirth_[] = $value; }
    public function addAllWarningMarriageDateBeforeBirth(array $values) { foreach($values as $value) {$this->warningMarriageDateBeforeBirth_[] = $value;} }
    
    // repeated .geneweb.api.object.WarningMotherDeadBeforeChildBirth warning_mother_dead_before_child_birth = 12;

    private $warningMotherDeadBeforeChildBirth_ = null;
    public function clearWarningMotherDeadBeforeChildBirth() { $this->warningMotherDeadBeforeChildBirth_ = null; }
    public function getWarningMotherDeadBeforeChildBirthCount() { if ($this->warningMotherDeadBeforeChildBirth_ === null ) return 0; else return count($this->warningMotherDeadBeforeChildBirth_); }
    public function getWarningMotherDeadBeforeChildBirth($index) { return $this->warningMotherDeadBeforeChildBirth_[$index]; }
    public function getWarningMotherDeadBeforeChildBirthArray() { if ($this->warningMotherDeadBeforeChildBirth_ === null ) return array(); else return $this->warningMotherDeadBeforeChildBirth_; }
    public function setWarningMotherDeadBeforeChildBirth($index, $value) {$this->warningMotherDeadBeforeChildBirth_[$index] = $value;	}
    public function addWarningMotherDeadBeforeChildBirth($value) { $this->warningMotherDeadBeforeChildBirth_[] = $value; }
    public function addAllWarningMotherDeadBeforeChildBirth(array $values) { foreach($values as $value) {$this->warningMotherDeadBeforeChildBirth_[] = $value;} }
    
    // repeated .geneweb.api.object.WarningParentBornAfterChild warning_parent_born_after_child = 13;

    private $warningParentBornAfterChild_ = null;
    public function clearWarningParentBornAfterChild() { $this->warningParentBornAfterChild_ = null; }
    public function getWarningParentBornAfterChildCount() { if ($this->warningParentBornAfterChild_ === null ) return 0; else return count($this->warningParentBornAfterChild_); }
    public function getWarningParentBornAfterChild($index) { return $this->warningParentBornAfterChild_[$index]; }
    public function getWarningParentBornAfterChildArray() { if ($this->warningParentBornAfterChild_ === null ) return array(); else return $this->warningParentBornAfterChild_; }
    public function setWarningParentBornAfterChild($index, $value) {$this->warningParentBornAfterChild_[$index] = $value;	}
    public function addWarningParentBornAfterChild($value) { $this->warningParentBornAfterChild_[] = $value; }
    public function addAllWarningParentBornAfterChild(array $values) { foreach($values as $value) {$this->warningParentBornAfterChild_[] = $value;} }
    
    // repeated .geneweb.api.object.WarningParentTooYoung warning_parent_too_young = 14;

    private $warningParentTooYoung_ = null;
    public function clearWarningParentTooYoung() { $this->warningParentTooYoung_ = null; }
    public function getWarningParentTooYoungCount() { if ($this->warningParentTooYoung_ === null ) return 0; else return count($this->warningParentTooYoung_); }
    public function getWarningParentTooYoung($index) { return $this->warningParentTooYoung_[$index]; }
    public function getWarningParentTooYoungArray() { if ($this->warningParentTooYoung_ === null ) return array(); else return $this->warningParentTooYoung_; }
    public function setWarningParentTooYoung($index, $value) {$this->warningParentTooYoung_[$index] = $value;	}
    public function addWarningParentTooYoung($value) { $this->warningParentTooYoung_[] = $value; }
    public function addAllWarningParentTooYoung(array $values) { foreach($values as $value) {$this->warningParentTooYoung_[] = $value;} }
    
    // repeated .geneweb.api.object.WarningTitleDatesError warning_title_dates_error = 15;

    private $warningTitleDatesError_ = null;
    public function clearWarningTitleDatesError() { $this->warningTitleDatesError_ = null; }
    public function getWarningTitleDatesErrorCount() { if ($this->warningTitleDatesError_ === null ) return 0; else return count($this->warningTitleDatesError_); }
    public function getWarningTitleDatesError($index) { return $this->warningTitleDatesError_[$index]; }
    public function getWarningTitleDatesErrorArray() { if ($this->warningTitleDatesError_ === null ) return array(); else return $this->warningTitleDatesError_; }
    public function setWarningTitleDatesError($index, $value) {$this->warningTitleDatesError_[$index] = $value;	}
    public function addWarningTitleDatesError($value) { $this->warningTitleDatesError_[] = $value; }
    public function addAllWarningTitleDatesError(array $values) { foreach($values as $value) {$this->warningTitleDatesError_[] = $value;} }
    
    // repeated .geneweb.api.object.WarningUndefinedSex warning_undefined_sex = 16;

    private $warningUndefinedSex_ = null;
    public function clearWarningUndefinedSex() { $this->warningUndefinedSex_ = null; }
    public function getWarningUndefinedSexCount() { if ($this->warningUndefinedSex_ === null ) return 0; else return count($this->warningUndefinedSex_); }
    public function getWarningUndefinedSex($index) { return $this->warningUndefinedSex_[$index]; }
    public function getWarningUndefinedSexArray() { if ($this->warningUndefinedSex_ === null ) return array(); else return $this->warningUndefinedSex_; }
    public function setWarningUndefinedSex($index, $value) {$this->warningUndefinedSex_[$index] = $value;	}
    public function addWarningUndefinedSex($value) { $this->warningUndefinedSex_[] = $value; }
    public function addAllWarningUndefinedSex(array $values) { foreach($values as $value) {$this->warningUndefinedSex_[] = $value;} }
    
    // repeated .geneweb.api.object.WarningYoungForMarriage warning_young_for_marriage = 17;

    private $warningYoungForMarriage_ = null;
    public function clearWarningYoungForMarriage() { $this->warningYoungForMarriage_ = null; }
    public function getWarningYoungForMarriageCount() { if ($this->warningYoungForMarriage_ === null ) return 0; else return count($this->warningYoungForMarriage_); }
    public function getWarningYoungForMarriage($index) { return $this->warningYoungForMarriage_[$index]; }
    public function getWarningYoungForMarriageArray() { if ($this->warningYoungForMarriage_ === null ) return array(); else return $this->warningYoungForMarriage_; }
    public function setWarningYoungForMarriage($index, $value) {$this->warningYoungForMarriage_[$index] = $value;	}
    public function addWarningYoungForMarriage($value) { $this->warningYoungForMarriage_[] = $value; }
    public function addAllWarningYoungForMarriage(array $values) { foreach($values as $value) {$this->warningYoungForMarriage_[] = $value;} }
    
    // repeated .geneweb.api.object.WarningCloseChildren warning_close_children = 18;

    private $warningCloseChildren_ = null;
    public function clearWarningCloseChildren() { $this->warningCloseChildren_ = null; }
    public function getWarningCloseChildrenCount() { if ($this->warningCloseChildren_ === null ) return 0; else return count($this->warningCloseChildren_); }
    public function getWarningCloseChildren($index) { return $this->warningCloseChildren_[$index]; }
    public function getWarningCloseChildrenArray() { if ($this->warningCloseChildren_ === null ) return array(); else return $this->warningCloseChildren_; }
    public function setWarningCloseChildren($index, $value) {$this->warningCloseChildren_[$index] = $value;	}
    public function addWarningCloseChildren($value) { $this->warningCloseChildren_[] = $value; }
    public function addAllWarningCloseChildren(array $values) { foreach($values as $value) {$this->warningCloseChildren_[] = $value;} }
    
    // repeated .geneweb.api.object.WarningParentTooOld warning_parent_too_old = 19;

    private $warningParentTooOld_ = null;
    public function clearWarningParentTooOld() { $this->warningParentTooOld_ = null; }
    public function getWarningParentTooOldCount() { if ($this->warningParentTooOld_ === null ) return 0; else return count($this->warningParentTooOld_); }
    public function getWarningParentTooOld($index) { return $this->warningParentTooOld_[$index]; }
    public function getWarningParentTooOldArray() { if ($this->warningParentTooOld_ === null ) return array(); else return $this->warningParentTooOld_; }
    public function setWarningParentTooOld($index, $value) {$this->warningParentTooOld_[$index] = $value;	}
    public function addWarningParentTooOld($value) { $this->warningParentTooOld_[] = $value; }
    public function addAllWarningParentTooOld(array $values) { foreach($values as $value) {$this->warningParentTooOld_[] = $value;} }
    
    // repeated .geneweb.api.object.WarningChangedOrderOfMarriages warning_changed_order_of_marriages = 20;

    private $warningChangedOrderOfMarriages_ = null;
    public function clearWarningChangedOrderOfMarriages() { $this->warningChangedOrderOfMarriages_ = null; }
    public function getWarningChangedOrderOfMarriagesCount() { if ($this->warningChangedOrderOfMarriages_ === null ) return 0; else return count($this->warningChangedOrderOfMarriages_); }
    public function getWarningChangedOrderOfMarriages($index) { return $this->warningChangedOrderOfMarriages_[$index]; }
    public function getWarningChangedOrderOfMarriagesArray() { if ($this->warningChangedOrderOfMarriages_ === null ) return array(); else return $this->warningChangedOrderOfMarriages_; }
    public function setWarningChangedOrderOfMarriages($index, $value) {$this->warningChangedOrderOfMarriages_[$index] = $value;	}
    public function addWarningChangedOrderOfMarriages($value) { $this->warningChangedOrderOfMarriages_[] = $value; }
    public function addAllWarningChangedOrderOfMarriages(array $values) { foreach($values as $value) {$this->warningChangedOrderOfMarriages_[] = $value;} }
    
    // repeated .geneweb.api.object.WarningBigAgeBetweenSpouses warning_big_age_between_spouses = 21;

    private $warningBigAgeBetweenSpouses_ = null;
    public function clearWarningBigAgeBetweenSpouses() { $this->warningBigAgeBetweenSpouses_ = null; }
    public function getWarningBigAgeBetweenSpousesCount() { if ($this->warningBigAgeBetweenSpouses_ === null ) return 0; else return count($this->warningBigAgeBetweenSpouses_); }
    public function getWarningBigAgeBetweenSpouses($index) { return $this->warningBigAgeBetweenSpouses_[$index]; }
    public function getWarningBigAgeBetweenSpousesArray() { if ($this->warningBigAgeBetweenSpouses_ === null ) return array(); else return $this->warningBigAgeBetweenSpouses_; }
    public function setWarningBigAgeBetweenSpouses($index, $value) {$this->warningBigAgeBetweenSpouses_[$index] = $value;	}
    public function addWarningBigAgeBetweenSpouses($value) { $this->warningBigAgeBetweenSpouses_[] = $value; }
    public function addAllWarningBigAgeBetweenSpouses(array $values) { foreach($values as $value) {$this->warningBigAgeBetweenSpouses_[] = $value;} }
    
    // repeated .geneweb.api.object.WarningDeadOld warning_dead_old = 22;

    private $warningDeadOld_ = null;
    public function clearWarningDeadOld() { $this->warningDeadOld_ = null; }
    public function getWarningDeadOldCount() { if ($this->warningDeadOld_ === null ) return 0; else return count($this->warningDeadOld_); }
    public function getWarningDeadOld($index) { return $this->warningDeadOld_[$index]; }
    public function getWarningDeadOldArray() { if ($this->warningDeadOld_ === null ) return array(); else return $this->warningDeadOld_; }
    public function setWarningDeadOld($index, $value) {$this->warningDeadOld_[$index] = $value;	}
    public function addWarningDeadOld($value) { $this->warningDeadOld_[] = $value; }
    public function addAllWarningDeadOld(array $values) { foreach($values as $value) {$this->warningDeadOld_[] = $value;} }
    
    // repeated .geneweb.api.object.WarningOldIndividual warning_old_individual = 23;

    private $warningOldIndividual_ = null;
    public function clearWarningOldIndividual() { $this->warningOldIndividual_ = null; }
    public function getWarningOldIndividualCount() { if ($this->warningOldIndividual_ === null ) return 0; else return count($this->warningOldIndividual_); }
    public function getWarningOldIndividual($index) { return $this->warningOldIndividual_[$index]; }
    public function getWarningOldIndividualArray() { if ($this->warningOldIndividual_ === null ) return array(); else return $this->warningOldIndividual_; }
    public function setWarningOldIndividual($index, $value) {$this->warningOldIndividual_[$index] = $value;	}
    public function addWarningOldIndividual($value) { $this->warningOldIndividual_[] = $value; }
    public function addAllWarningOldIndividual(array $values) { foreach($values as $value) {$this->warningOldIndividual_[] = $value;} }
    
    // repeated .geneweb.api.object.WarningWitnessDateAfterDeath warning_witness_date_after_death = 24;

    private $warningWitnessDateAfterDeath_ = null;
    public function clearWarningWitnessDateAfterDeath() { $this->warningWitnessDateAfterDeath_ = null; }
    public function getWarningWitnessDateAfterDeathCount() { if ($this->warningWitnessDateAfterDeath_ === null ) return 0; else return count($this->warningWitnessDateAfterDeath_); }
    public function getWarningWitnessDateAfterDeath($index) { return $this->warningWitnessDateAfterDeath_[$index]; }
    public function getWarningWitnessDateAfterDeathArray() { if ($this->warningWitnessDateAfterDeath_ === null ) return array(); else return $this->warningWitnessDateAfterDeath_; }
    public function setWarningWitnessDateAfterDeath($index, $value) {$this->warningWitnessDateAfterDeath_[$index] = $value;	}
    public function addWarningWitnessDateAfterDeath($value) { $this->warningWitnessDateAfterDeath_[] = $value; }
    public function addAllWarningWitnessDateAfterDeath(array $values) { foreach($values as $value) {$this->warningWitnessDateAfterDeath_[] = $value;} }
    
    // repeated .geneweb.api.object.WarningWitnessDateBeforeBirth warning_witness_date_before_birth = 25;

    private $warningWitnessDateBeforeBirth_ = null;
    public function clearWarningWitnessDateBeforeBirth() { $this->warningWitnessDateBeforeBirth_ = null; }
    public function getWarningWitnessDateBeforeBirthCount() { if ($this->warningWitnessDateBeforeBirth_ === null ) return 0; else return count($this->warningWitnessDateBeforeBirth_); }
    public function getWarningWitnessDateBeforeBirth($index) { return $this->warningWitnessDateBeforeBirth_[$index]; }
    public function getWarningWitnessDateBeforeBirthArray() { if ($this->warningWitnessDateBeforeBirth_ === null ) return array(); else return $this->warningWitnessDateBeforeBirth_; }
    public function setWarningWitnessDateBeforeBirth($index, $value) {$this->warningWitnessDateBeforeBirth_[$index] = $value;	}
    public function addWarningWitnessDateBeforeBirth($value) { $this->warningWitnessDateBeforeBirth_[] = $value; }
    public function addAllWarningWitnessDateBeforeBirth(array $values) { foreach($values as $value) {$this->warningWitnessDateBeforeBirth_[] = $value;} }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.BaseWarnings)
  }
  
  // message geneweb.api.object.FilterDate
  class FilterDate {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\FilterDate: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->day_ = $tmp;
            
            break;
          case 2:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->month_ = $tmp;
            
            break;
          case 3:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->year_ = $tmp;
            
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->day_)) {
        fwrite($fp, "\x08");
        \Protobuf::write_varint($fp, $this->day_);
      }
      if (!is_null($this->month_)) {
        fwrite($fp, "\x10");
        \Protobuf::write_varint($fp, $this->month_);
      }
      if (!is_null($this->year_)) {
        fwrite($fp, "\x18");
        \Protobuf::write_varint($fp, $this->year_);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->day_)) {
        $size += 1 + \Protobuf::size_varint($this->day_);
      }
      if (!is_null($this->month_)) {
        $size += 1 + \Protobuf::size_varint($this->month_);
      }
      if (!is_null($this->year_)) {
        $size += 1 + \Protobuf::size_varint($this->year_);
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->day_ === null) return false;
      if ($this->month_ === null) return false;
      if ($this->year_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('day_', $this->day_)
           . \Protobuf::toString('month_', $this->month_)
           . \Protobuf::toString('year_', $this->year_);
    }
    
    // required int32 day = 1;

    private $day_ = null;
    public function clearDay() { $this->day_ = null; }
    public function hasDay() { return $this->day_ !== null; }
    public function getDay() { if($this->day_ === null) return 0; else return $this->day_; }
    public function setDay($value) { $this->day_ = $value; }
    
    // required int32 month = 2;

    private $month_ = null;
    public function clearMonth() { $this->month_ = null; }
    public function hasMonth() { return $this->month_ !== null; }
    public function getMonth() { if($this->month_ === null) return 0; else return $this->month_; }
    public function setMonth($value) { $this->month_ = $value; }
    
    // required int32 year = 3;

    private $year_ = null;
    public function clearYear() { $this->year_ = null; }
    public function hasYear() { return $this->year_ !== null; }
    public function getYear() { if($this->year_ === null) return 0; else return $this->year_; }
    public function setYear($value) { $this->year_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.FilterDate)
  }
  
  // message geneweb.api.object.FilterDateRange
  class FilterDateRange {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\FilterDateRange: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->dateBegin_ = new \geneweb\api\object\FilterDate($fp, $len);
            ASSERT('$len == 0');
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->dateEnd_ = new \geneweb\api\object\FilterDate($fp, $len);
            ASSERT('$len == 0');
            break;
          case 3:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->onlyExact_ = $tmp > 0 ? true : false;
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->dateBegin_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, $this->dateBegin_->size()); // message
        $this->dateBegin_->write($fp);
      }
      if (!is_null($this->dateEnd_)) {
        fwrite($fp, "\x12");
        \Protobuf::write_varint($fp, $this->dateEnd_->size()); // message
        $this->dateEnd_->write($fp);
      }
      if (!is_null($this->onlyExact_)) {
        fwrite($fp, "\x18");
        \Protobuf::write_varint($fp, $this->onlyExact_ ? 1 : 0);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->dateBegin_)) {
        $l = $this->dateBegin_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->dateEnd_)) {
        $l = $this->dateEnd_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->onlyExact_)) {
        $size += 2;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->dateBegin_ === null) return false;
      if ($this->dateEnd_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('dateBegin_', $this->dateBegin_)
           . \Protobuf::toString('dateEnd_', $this->dateEnd_)
           . \Protobuf::toString('onlyExact_', $this->onlyExact_);
    }
    
    // required .geneweb.api.object.FilterDate date_begin = 1;

    private $dateBegin_ = null;
    public function clearDateBegin() { $this->dateBegin_ = null; }
    public function hasDateBegin() { return $this->dateBegin_ !== null; }
    public function getDateBegin() { if($this->dateBegin_ === null) return null; else return $this->dateBegin_; }
    public function setDateBegin(\geneweb\api\object\FilterDate $value) { $this->dateBegin_ = $value; }
    
    // required .geneweb.api.object.FilterDate date_end = 2;

    private $dateEnd_ = null;
    public function clearDateEnd() { $this->dateEnd_ = null; }
    public function hasDateEnd() { return $this->dateEnd_ !== null; }
    public function getDateEnd() { if($this->dateEnd_ === null) return null; else return $this->dateEnd_; }
    public function setDateEnd(\geneweb\api\object\FilterDate $value) { $this->dateEnd_ = $value; }
    
    // optional bool only_exact = 3 [default = false];

    private $onlyExact_ = null;
    public function clearOnlyExact() { $this->onlyExact_ = null; }
    public function hasOnlyExact() { return $this->onlyExact_ !== null; }
    public function getOnlyExact() { if($this->onlyExact_ === null) return false; else return $this->onlyExact_; }
    public function setOnlyExact($value) { $this->onlyExact_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.FilterDateRange)
  }
  
  // message geneweb.api.object.Filters
  class Filters {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\Filters: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->onlySosa_ = $tmp > 0 ? true : false;
            break;
          case 2:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->onlyRecent_ = $tmp > 0 ? true : false;
            break;
          case 3:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->sex_ = $tmp;
            
            break;
          case 4:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->nbResults_ = $tmp > 0 ? true : false;
            break;
          case 5:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->dateBirth_ = new \geneweb\api\object\FilterDateRange($fp, $len);
            ASSERT('$len == 0');
            break;
          case 6:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->dateDeath_ = new \geneweb\api\object\FilterDateRange($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->onlySosa_)) {
        fwrite($fp, "\x08");
        \Protobuf::write_varint($fp, $this->onlySosa_ ? 1 : 0);
      }
      if (!is_null($this->onlyRecent_)) {
        fwrite($fp, "\x10");
        \Protobuf::write_varint($fp, $this->onlyRecent_ ? 1 : 0);
      }
      if (!is_null($this->sex_)) {
        fwrite($fp, "\x18");
        \Protobuf::write_varint($fp, $this->sex_);
      }
      if (!is_null($this->nbResults_)) {
        fwrite($fp, " ");
        \Protobuf::write_varint($fp, $this->nbResults_ ? 1 : 0);
      }
      if (!is_null($this->dateBirth_)) {
        fwrite($fp, "*");
        \Protobuf::write_varint($fp, $this->dateBirth_->size()); // message
        $this->dateBirth_->write($fp);
      }
      if (!is_null($this->dateDeath_)) {
        fwrite($fp, "2");
        \Protobuf::write_varint($fp, $this->dateDeath_->size()); // message
        $this->dateDeath_->write($fp);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->onlySosa_)) {
        $size += 2;
      }
      if (!is_null($this->onlyRecent_)) {
        $size += 2;
      }
      if (!is_null($this->sex_)) {
        $size += 1 + \Protobuf::size_varint($this->sex_);
      }
      if (!is_null($this->nbResults_)) {
        $size += 2;
      }
      if (!is_null($this->dateBirth_)) {
        $l = $this->dateBirth_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->dateDeath_)) {
        $l = $this->dateDeath_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }
    
    public function validateRequired() {
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('onlySosa_', $this->onlySosa_)
           . \Protobuf::toString('onlyRecent_', $this->onlyRecent_)
           . \Protobuf::toString('sex_', \geneweb\api\object\Sex::toString($this->sex_))
           . \Protobuf::toString('nbResults_', $this->nbResults_)
           . \Protobuf::toString('dateBirth_', $this->dateBirth_)
           . \Protobuf::toString('dateDeath_', $this->dateDeath_);
    }
    
    // optional bool only_sosa = 1 [default = false];

    private $onlySosa_ = null;
    public function clearOnlySosa() { $this->onlySosa_ = null; }
    public function hasOnlySosa() { return $this->onlySosa_ !== null; }
    public function getOnlySosa() { if($this->onlySosa_ === null) return false; else return $this->onlySosa_; }
    public function setOnlySosa($value) { $this->onlySosa_ = $value; }
    
    // optional bool only_recent = 2 [default = false];

    private $onlyRecent_ = null;
    public function clearOnlyRecent() { $this->onlyRecent_ = null; }
    public function hasOnlyRecent() { return $this->onlyRecent_ !== null; }
    public function getOnlyRecent() { if($this->onlyRecent_ === null) return false; else return $this->onlyRecent_; }
    public function setOnlyRecent($value) { $this->onlyRecent_ = $value; }
    
    // optional .geneweb.api.object.Sex sex = 3;

    private $sex_ = null;
    public function clearSex() { $this->sex_ = null; }
    public function hasSex() { return $this->sex_ !== null; }
    public function getSex() { if($this->sex_ === null) return \geneweb\api\object\Sex::MALE; else return $this->sex_; }
    public function setSex($value) { $this->sex_ = $value; }
    
    // optional bool nb_results = 4 [default = false];

    private $nbResults_ = null;
    public function clearNbResults() { $this->nbResults_ = null; }
    public function hasNbResults() { return $this->nbResults_ !== null; }
    public function getNbResults() { if($this->nbResults_ === null) return false; else return $this->nbResults_; }
    public function setNbResults($value) { $this->nbResults_ = $value; }
    
    // optional .geneweb.api.object.FilterDateRange date_birth = 5;

    private $dateBirth_ = null;
    public function clearDateBirth() { $this->dateBirth_ = null; }
    public function hasDateBirth() { return $this->dateBirth_ !== null; }
    public function getDateBirth() { if($this->dateBirth_ === null) return null; else return $this->dateBirth_; }
    public function setDateBirth(\geneweb\api\object\FilterDateRange $value) { $this->dateBirth_ = $value; }
    
    // optional .geneweb.api.object.FilterDateRange date_death = 6;

    private $dateDeath_ = null;
    public function clearDateDeath() { $this->dateDeath_ = null; }
    public function hasDateDeath() { return $this->dateDeath_ !== null; }
    public function getDateDeath() { if($this->dateDeath_ === null) return null; else return $this->dateDeath_; }
    public function setDateDeath(\geneweb\api\object\FilterDateRange $value) { $this->dateDeath_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.Filters)
  }
  
  // message geneweb.api.object.ModificationStatus
  class ModificationStatus {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\ModificationStatus: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->status_ = $tmp > 0 ? true : false;
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->baseWarnings_ = new \geneweb\api\object\BaseWarnings($fp, $len);
            ASSERT('$len == 0');
            break;
          case 3:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->index_ = $tmp;
            
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->status_)) {
        fwrite($fp, "\x08");
        \Protobuf::write_varint($fp, $this->status_ ? 1 : 0);
      }
      if (!is_null($this->baseWarnings_)) {
        fwrite($fp, "\x12");
        \Protobuf::write_varint($fp, $this->baseWarnings_->size()); // message
        $this->baseWarnings_->write($fp);
      }
      if (!is_null($this->index_)) {
        fwrite($fp, "\x18");
        \Protobuf::write_varint($fp, $this->index_);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->status_)) {
        $size += 2;
      }
      if (!is_null($this->baseWarnings_)) {
        $l = $this->baseWarnings_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->index_)) {
        $size += 1 + \Protobuf::size_varint($this->index_);
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->status_ === null) return false;
      if ($this->baseWarnings_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('status_', $this->status_)
           . \Protobuf::toString('baseWarnings_', $this->baseWarnings_)
           . \Protobuf::toString('index_', $this->index_);
    }
    
    // required bool status = 1;

    private $status_ = null;
    public function clearStatus() { $this->status_ = null; }
    public function hasStatus() { return $this->status_ !== null; }
    public function getStatus() { if($this->status_ === null) return false; else return $this->status_; }
    public function setStatus($value) { $this->status_ = $value; }
    
    // required .geneweb.api.object.BaseWarnings base_warnings = 2;

    private $baseWarnings_ = null;
    public function clearBaseWarnings() { $this->baseWarnings_ = null; }
    public function hasBaseWarnings() { return $this->baseWarnings_ !== null; }
    public function getBaseWarnings() { if($this->baseWarnings_ === null) return null; else return $this->baseWarnings_; }
    public function setBaseWarnings(\geneweb\api\object\BaseWarnings $value) { $this->baseWarnings_ = $value; }
    
    // optional int32 index = 3;

    private $index_ = null;
    public function clearIndex() { $this->index_ = null; }
    public function hasIndex() { return $this->index_ !== null; }
    public function getIndex() { if($this->index_ === null) return 0; else return $this->index_; }
    public function setIndex($value) { $this->index_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.ModificationStatus)
  }
  
  // message geneweb.api.object.CorrespondanceFamily
  class CorrespondanceFamily {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\CorrespondanceFamily: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->index_ = $tmp;
            
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->spouse_ = new \geneweb\api\object\Person($fp, $len);
            ASSERT('$len == 0');
            break;
          case 3:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->children_[] = new \geneweb\api\object\Person($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->index_)) {
        fwrite($fp, "\x08");
        \Protobuf::write_varint($fp, $this->index_);
      }
      if (!is_null($this->spouse_)) {
        fwrite($fp, "\x12");
        \Protobuf::write_varint($fp, $this->spouse_->size()); // message
        $this->spouse_->write($fp);
      }
      if (!is_null($this->children_))
        foreach($this->children_ as $v) {
          fwrite($fp, "\x1a");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->index_)) {
        $size += 1 + \Protobuf::size_varint($this->index_);
      }
      if (!is_null($this->spouse_)) {
        $l = $this->spouse_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->children_))
        foreach($this->children_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->index_ === null) return false;
      if ($this->spouse_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('index_', $this->index_)
           . \Protobuf::toString('spouse_', $this->spouse_)
           . \Protobuf::toString('children_', $this->children_);
    }
    
    // required int32 index = 1;

    private $index_ = null;
    public function clearIndex() { $this->index_ = null; }
    public function hasIndex() { return $this->index_ !== null; }
    public function getIndex() { if($this->index_ === null) return 0; else return $this->index_; }
    public function setIndex($value) { $this->index_ = $value; }
    
    // required .geneweb.api.object.Person spouse = 2;

    private $spouse_ = null;
    public function clearSpouse() { $this->spouse_ = null; }
    public function hasSpouse() { return $this->spouse_ !== null; }
    public function getSpouse() { if($this->spouse_ === null) return null; else return $this->spouse_; }
    public function setSpouse(\geneweb\api\object\Person $value) { $this->spouse_ = $value; }
    
    // repeated .geneweb.api.object.Person children = 3;

    private $children_ = null;
    public function clearChildren() { $this->children_ = null; }
    public function getChildrenCount() { if ($this->children_ === null ) return 0; else return count($this->children_); }
    public function getChildren($index) { return $this->children_[$index]; }
    public function getChildrenArray() { if ($this->children_ === null ) return array(); else return $this->children_; }
    public function setChildren($index, $value) {$this->children_[$index] = $value;	}
    public function addChildren($value) { $this->children_[] = $value; }
    public function addAllChildren(array $values) { foreach($values as $value) {$this->children_[] = $value;} }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.CorrespondanceFamily)
  }
  
  // message geneweb.api.object.Correspondance
  class Correspondance {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\Correspondance: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->base_ = $tmp;
            $limit-=$len;
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->person_ = new \geneweb\api\object\Person($fp, $len);
            ASSERT('$len == 0');
            break;
          case 3:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->father_ = new \geneweb\api\object\Person($fp, $len);
            ASSERT('$len == 0');
            break;
          case 4:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->mother_ = new \geneweb\api\object\Person($fp, $len);
            ASSERT('$len == 0');
            break;
          case 5:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->families_[] = new \geneweb\api\object\CorrespondanceFamily($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->base_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, strlen($this->base_));
        fwrite($fp, $this->base_);
      }
      if (!is_null($this->person_)) {
        fwrite($fp, "\x12");
        \Protobuf::write_varint($fp, $this->person_->size()); // message
        $this->person_->write($fp);
      }
      if (!is_null($this->father_)) {
        fwrite($fp, "\x1a");
        \Protobuf::write_varint($fp, $this->father_->size()); // message
        $this->father_->write($fp);
      }
      if (!is_null($this->mother_)) {
        fwrite($fp, "\"");
        \Protobuf::write_varint($fp, $this->mother_->size()); // message
        $this->mother_->write($fp);
      }
      if (!is_null($this->families_))
        foreach($this->families_ as $v) {
          fwrite($fp, "*");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->base_)) {
        $l = strlen($this->base_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->person_)) {
        $l = $this->person_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->father_)) {
        $l = $this->father_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->mother_)) {
        $l = $this->mother_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->families_))
        foreach($this->families_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->base_ === null) return false;
      if ($this->person_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('base_', $this->base_)
           . \Protobuf::toString('person_', $this->person_)
           . \Protobuf::toString('father_', $this->father_)
           . \Protobuf::toString('mother_', $this->mother_)
           . \Protobuf::toString('families_', $this->families_);
    }
    
    // required string base = 1;

    private $base_ = null;
    public function clearBase() { $this->base_ = null; }
    public function hasBase() { return $this->base_ !== null; }
    public function getBase() { if($this->base_ === null) return ""; else return $this->base_; }
    public function setBase($value) { $this->base_ = $value; }
    
    // required .geneweb.api.object.Person person = 2;

    private $person_ = null;
    public function clearPerson() { $this->person_ = null; }
    public function hasPerson() { return $this->person_ !== null; }
    public function getPerson() { if($this->person_ === null) return null; else return $this->person_; }
    public function setPerson(\geneweb\api\object\Person $value) { $this->person_ = $value; }
    
    // optional .geneweb.api.object.Person father = 3;

    private $father_ = null;
    public function clearFather() { $this->father_ = null; }
    public function hasFather() { return $this->father_ !== null; }
    public function getFather() { if($this->father_ === null) return null; else return $this->father_; }
    public function setFather(\geneweb\api\object\Person $value) { $this->father_ = $value; }
    
    // optional .geneweb.api.object.Person mother = 4;

    private $mother_ = null;
    public function clearMother() { $this->mother_ = null; }
    public function hasMother() { return $this->mother_ !== null; }
    public function getMother() { if($this->mother_ === null) return null; else return $this->mother_; }
    public function setMother(\geneweb\api\object\Person $value) { $this->mother_ = $value; }
    
    // repeated .geneweb.api.object.CorrespondanceFamily families = 5;

    private $families_ = null;
    public function clearFamilies() { $this->families_ = null; }
    public function getFamiliesCount() { if ($this->families_ === null ) return 0; else return count($this->families_); }
    public function getFamilies($index) { return $this->families_[$index]; }
    public function getFamiliesArray() { if ($this->families_ === null ) return array(); else return $this->families_; }
    public function setFamilies($index, $value) {$this->families_[$index] = $value;	}
    public function addFamilies($value) { $this->families_[] = $value; }
    public function addAllFamilies(array $values) { foreach($values as $value) {$this->families_[] = $value;} }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.Correspondance)
  }
  
  // message geneweb.api.object.CorrespondanceList
  class CorrespondanceList {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\CorrespondanceList: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->correspondances_[] = new \geneweb\api\object\Correspondance($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->correspondances_))
        foreach($this->correspondances_ as $v) {
          fwrite($fp, "\x0a");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->correspondances_))
        foreach($this->correspondances_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      return $size;
    }
    
    public function validateRequired() {
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('correspondances_', $this->correspondances_);
    }
    
    // repeated .geneweb.api.object.Correspondance correspondances = 1;

    private $correspondances_ = null;
    public function clearCorrespondances() { $this->correspondances_ = null; }
    public function getCorrespondancesCount() { if ($this->correspondances_ === null ) return 0; else return count($this->correspondances_); }
    public function getCorrespondances($index) { return $this->correspondances_[$index]; }
    public function getCorrespondancesArray() { if ($this->correspondances_ === null ) return array(); else return $this->correspondances_; }
    public function setCorrespondances($index, $value) {$this->correspondances_[$index] = $value;	}
    public function addCorrespondances($value) { $this->correspondances_[] = $value; }
    public function addAllCorrespondances(array $values) { foreach($values as $value) {$this->correspondances_[] = $value;} }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.CorrespondanceList)
  }
  
  // message geneweb.api.object.NotificationBirthdayParams
  class NotificationBirthdayParams {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\NotificationBirthdayParams: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->person_ = new \geneweb\api\object\ReferencePerson($fp, $len);
            ASSERT('$len == 0');
            break;
          case 2:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->params_ = $tmp;
            
            break;
          case 3:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->month_ = $tmp;
            
            break;
          case 4:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->day_ = $tmp;
            
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->person_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, $this->person_->size()); // message
        $this->person_->write($fp);
      }
      if (!is_null($this->params_)) {
        fwrite($fp, "\x10");
        \Protobuf::write_varint($fp, $this->params_);
      }
      if (!is_null($this->month_)) {
        fwrite($fp, "\x18");
        \Protobuf::write_varint($fp, $this->month_);
      }
      if (!is_null($this->day_)) {
        fwrite($fp, " ");
        \Protobuf::write_varint($fp, $this->day_);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->person_)) {
        $l = $this->person_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->params_)) {
        $size += 1 + \Protobuf::size_varint($this->params_);
      }
      if (!is_null($this->month_)) {
        $size += 1 + \Protobuf::size_varint($this->month_);
      }
      if (!is_null($this->day_)) {
        $size += 1 + \Protobuf::size_varint($this->day_);
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->person_ === null) return false;
      if ($this->params_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('person_', $this->person_)
           . \Protobuf::toString('params_', \geneweb\api\object\NotifBirthdayParams::toString($this->params_))
           . \Protobuf::toString('month_', $this->month_)
           . \Protobuf::toString('day_', $this->day_);
    }
    
    // required .geneweb.api.object.ReferencePerson person = 1;

    private $person_ = null;
    public function clearPerson() { $this->person_ = null; }
    public function hasPerson() { return $this->person_ !== null; }
    public function getPerson() { if($this->person_ === null) return null; else return $this->person_; }
    public function setPerson(\geneweb\api\object\ReferencePerson $value) { $this->person_ = $value; }
    
    // required .geneweb.api.object.NotifBirthdayParams params = 2;

    private $params_ = null;
    public function clearParams() { $this->params_ = null; }
    public function hasParams() { return $this->params_ !== null; }
    public function getParams() { if($this->params_ === null) return \geneweb\api\object\NotifBirthdayParams::CLOSE_PERSON; else return $this->params_; }
    public function setParams($value) { $this->params_ = $value; }
    
    // optional int32 month = 3;

    private $month_ = null;
    public function clearMonth() { $this->month_ = null; }
    public function hasMonth() { return $this->month_ !== null; }
    public function getMonth() { if($this->month_ === null) return 0; else return $this->month_; }
    public function setMonth($value) { $this->month_ = $value; }
    
    // optional int32 day = 4;

    private $day_ = null;
    public function clearDay() { $this->day_ = null; }
    public function hasDay() { return $this->day_ !== null; }
    public function getDay() { if($this->day_ === null) return 0; else return $this->day_; }
    public function setDay($value) { $this->day_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.NotificationBirthdayParams)
  }
  
  // message geneweb.api.object.NotificationBirthday
  class NotificationBirthday {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\NotificationBirthday: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->number_ = $tmp;
            
            break;
          case 2:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->hasProprioBirthday_ = $tmp > 0 ? true : false;
            break;
          case 3:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->firstname1_ = $tmp;
            $limit-=$len;
            break;
          case 4:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->firstname2_ = $tmp;
            $limit-=$len;
            break;
          case 5:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->firstname3_ = $tmp;
            $limit-=$len;
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->number_)) {
        fwrite($fp, "\x08");
        \Protobuf::write_varint($fp, $this->number_);
      }
      if (!is_null($this->hasProprioBirthday_)) {
        fwrite($fp, "\x10");
        \Protobuf::write_varint($fp, $this->hasProprioBirthday_ ? 1 : 0);
      }
      if (!is_null($this->firstname1_)) {
        fwrite($fp, "\x1a");
        \Protobuf::write_varint($fp, strlen($this->firstname1_));
        fwrite($fp, $this->firstname1_);
      }
      if (!is_null($this->firstname2_)) {
        fwrite($fp, "\"");
        \Protobuf::write_varint($fp, strlen($this->firstname2_));
        fwrite($fp, $this->firstname2_);
      }
      if (!is_null($this->firstname3_)) {
        fwrite($fp, "*");
        \Protobuf::write_varint($fp, strlen($this->firstname3_));
        fwrite($fp, $this->firstname3_);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->number_)) {
        $size += 1 + \Protobuf::size_varint($this->number_);
      }
      if (!is_null($this->hasProprioBirthday_)) {
        $size += 2;
      }
      if (!is_null($this->firstname1_)) {
        $l = strlen($this->firstname1_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->firstname2_)) {
        $l = strlen($this->firstname2_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->firstname3_)) {
        $l = strlen($this->firstname3_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->number_ === null) return false;
      if ($this->hasProprioBirthday_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('number_', $this->number_)
           . \Protobuf::toString('hasProprioBirthday_', $this->hasProprioBirthday_)
           . \Protobuf::toString('firstname1_', $this->firstname1_)
           . \Protobuf::toString('firstname2_', $this->firstname2_)
           . \Protobuf::toString('firstname3_', $this->firstname3_);
    }
    
    // required int32 number = 1;

    private $number_ = null;
    public function clearNumber() { $this->number_ = null; }
    public function hasNumber() { return $this->number_ !== null; }
    public function getNumber() { if($this->number_ === null) return 0; else return $this->number_; }
    public function setNumber($value) { $this->number_ = $value; }
    
    // required bool has_proprio_birthday = 2;

    private $hasProprioBirthday_ = null;
    public function clearHasProprioBirthday() { $this->hasProprioBirthday_ = null; }
    public function hasHasProprioBirthday() { return $this->hasProprioBirthday_ !== null; }
    public function getHasProprioBirthday() { if($this->hasProprioBirthday_ === null) return false; else return $this->hasProprioBirthday_; }
    public function setHasProprioBirthday($value) { $this->hasProprioBirthday_ = $value; }
    
    // optional string firstname1 = 3;

    private $firstname1_ = null;
    public function clearFirstname1() { $this->firstname1_ = null; }
    public function hasFirstname1() { return $this->firstname1_ !== null; }
    public function getFirstname1() { if($this->firstname1_ === null) return ""; else return $this->firstname1_; }
    public function setFirstname1($value) { $this->firstname1_ = $value; }
    
    // optional string firstname2 = 4;

    private $firstname2_ = null;
    public function clearFirstname2() { $this->firstname2_ = null; }
    public function hasFirstname2() { return $this->firstname2_ !== null; }
    public function getFirstname2() { if($this->firstname2_ === null) return ""; else return $this->firstname2_; }
    public function setFirstname2($value) { $this->firstname2_ = $value; }
    
    // optional string firstname3 = 5;

    private $firstname3_ = null;
    public function clearFirstname3() { $this->firstname3_ = null; }
    public function hasFirstname3() { return $this->firstname3_ !== null; }
    public function getFirstname3() { if($this->firstname3_ === null) return ""; else return $this->firstname3_; }
    public function setFirstname3($value) { $this->firstname3_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.NotificationBirthday)
  }
  
  // message geneweb.api.object.PersonStart
  class PersonStart {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\PersonStart: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->lastname_ = $tmp;
            $limit-=$len;
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->firstname_ = $tmp;
            $limit-=$len;
            break;
          case 3:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->sex_ = $tmp;
            
            break;
          case 4:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->birthDateDay_ = $tmp;
            
            break;
          case 5:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->birthDateMonth_ = $tmp;
            
            break;
          case 6:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->birthDateYear_ = $tmp;
            
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->lastname_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, strlen($this->lastname_));
        fwrite($fp, $this->lastname_);
      }
      if (!is_null($this->firstname_)) {
        fwrite($fp, "\x12");
        \Protobuf::write_varint($fp, strlen($this->firstname_));
        fwrite($fp, $this->firstname_);
      }
      if (!is_null($this->sex_)) {
        fwrite($fp, "\x18");
        \Protobuf::write_varint($fp, $this->sex_);
      }
      if (!is_null($this->birthDateDay_)) {
        fwrite($fp, " ");
        \Protobuf::write_varint($fp, $this->birthDateDay_);
      }
      if (!is_null($this->birthDateMonth_)) {
        fwrite($fp, "(");
        \Protobuf::write_varint($fp, $this->birthDateMonth_);
      }
      if (!is_null($this->birthDateYear_)) {
        fwrite($fp, "0");
        \Protobuf::write_varint($fp, $this->birthDateYear_);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->lastname_)) {
        $l = strlen($this->lastname_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->firstname_)) {
        $l = strlen($this->firstname_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->sex_)) {
        $size += 1 + \Protobuf::size_varint($this->sex_);
      }
      if (!is_null($this->birthDateDay_)) {
        $size += 1 + \Protobuf::size_varint($this->birthDateDay_);
      }
      if (!is_null($this->birthDateMonth_)) {
        $size += 1 + \Protobuf::size_varint($this->birthDateMonth_);
      }
      if (!is_null($this->birthDateYear_)) {
        $size += 1 + \Protobuf::size_varint($this->birthDateYear_);
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->lastname_ === null) return false;
      if ($this->firstname_ === null) return false;
      if ($this->sex_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('lastname_', $this->lastname_)
           . \Protobuf::toString('firstname_', $this->firstname_)
           . \Protobuf::toString('sex_', \geneweb\api\object\Sex::toString($this->sex_))
           . \Protobuf::toString('birthDateDay_', $this->birthDateDay_)
           . \Protobuf::toString('birthDateMonth_', $this->birthDateMonth_)
           . \Protobuf::toString('birthDateYear_', $this->birthDateYear_);
    }
    
    // required string lastname = 1;

    private $lastname_ = null;
    public function clearLastname() { $this->lastname_ = null; }
    public function hasLastname() { return $this->lastname_ !== null; }
    public function getLastname() { if($this->lastname_ === null) return ""; else return $this->lastname_; }
    public function setLastname($value) { $this->lastname_ = $value; }
    
    // required string firstname = 2;

    private $firstname_ = null;
    public function clearFirstname() { $this->firstname_ = null; }
    public function hasFirstname() { return $this->firstname_ !== null; }
    public function getFirstname() { if($this->firstname_ === null) return ""; else return $this->firstname_; }
    public function setFirstname($value) { $this->firstname_ = $value; }
    
    // required .geneweb.api.object.Sex sex = 3;

    private $sex_ = null;
    public function clearSex() { $this->sex_ = null; }
    public function hasSex() { return $this->sex_ !== null; }
    public function getSex() { if($this->sex_ === null) return \geneweb\api\object\Sex::MALE; else return $this->sex_; }
    public function setSex($value) { $this->sex_ = $value; }
    
    // optional int32 birth_date_day = 4;

    private $birthDateDay_ = null;
    public function clearBirthDateDay() { $this->birthDateDay_ = null; }
    public function hasBirthDateDay() { return $this->birthDateDay_ !== null; }
    public function getBirthDateDay() { if($this->birthDateDay_ === null) return 0; else return $this->birthDateDay_; }
    public function setBirthDateDay($value) { $this->birthDateDay_ = $value; }
    
    // optional int32 birth_date_month = 5;

    private $birthDateMonth_ = null;
    public function clearBirthDateMonth() { $this->birthDateMonth_ = null; }
    public function hasBirthDateMonth() { return $this->birthDateMonth_ !== null; }
    public function getBirthDateMonth() { if($this->birthDateMonth_ === null) return 0; else return $this->birthDateMonth_; }
    public function setBirthDateMonth($value) { $this->birthDateMonth_ = $value; }
    
    // optional int32 birth_date_year = 6;

    private $birthDateYear_ = null;
    public function clearBirthDateYear() { $this->birthDateYear_ = null; }
    public function hasBirthDateYear() { return $this->birthDateYear_ !== null; }
    public function getBirthDateYear() { if($this->birthDateYear_ === null) return 0; else return $this->birthDateYear_; }
    public function setBirthDateYear($value) { $this->birthDateYear_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.PersonStart)
  }
  
  // message geneweb.api.object.SynchroParams
  class SynchroParams {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\SynchroParams: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->exportDirectory_ = $tmp;
            $limit-=$len;
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->timestamp_ = $tmp;
            $limit-=$len;
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->exportDirectory_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, strlen($this->exportDirectory_));
        fwrite($fp, $this->exportDirectory_);
      }
      if (!is_null($this->timestamp_)) {
        fwrite($fp, "\x12");
        \Protobuf::write_varint($fp, strlen($this->timestamp_));
        fwrite($fp, $this->timestamp_);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->exportDirectory_)) {
        $l = strlen($this->exportDirectory_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->timestamp_)) {
        $l = strlen($this->timestamp_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->exportDirectory_ === null) return false;
      if ($this->timestamp_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('exportDirectory_', $this->exportDirectory_)
           . \Protobuf::toString('timestamp_', $this->timestamp_);
    }
    
    // required string export_directory = 1;

    private $exportDirectory_ = null;
    public function clearExportDirectory() { $this->exportDirectory_ = null; }
    public function hasExportDirectory() { return $this->exportDirectory_ !== null; }
    public function getExportDirectory() { if($this->exportDirectory_ === null) return ""; else return $this->exportDirectory_; }
    public function setExportDirectory($value) { $this->exportDirectory_ = $value; }
    
    // required string timestamp = 2;

    private $timestamp_ = null;
    public function clearTimestamp() { $this->timestamp_ = null; }
    public function hasTimestamp() { return $this->timestamp_ !== null; }
    public function getTimestamp() { if($this->timestamp_ === null) return ""; else return $this->timestamp_; }
    public function setTimestamp($value) { $this->timestamp_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.SynchroParams)
  }
  
  // message geneweb.api.object.LastModifications
  class LastModifications {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\LastModifications: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->wizard_ = $tmp;
            $limit-=$len;
            break;
          case 2:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new \Exception('Protobuf::read_varint returned false');
            $this->maxRes_ = $tmp;
            
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->wizard_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, strlen($this->wizard_));
        fwrite($fp, $this->wizard_);
      }
      if (!is_null($this->maxRes_)) {
        fwrite($fp, "\x10");
        \Protobuf::write_varint($fp, $this->maxRes_);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->wizard_)) {
        $l = strlen($this->wizard_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->maxRes_)) {
        $size += 1 + \Protobuf::size_varint($this->maxRes_);
      }
      return $size;
    }
    
    public function validateRequired() {
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('wizard_', $this->wizard_)
           . \Protobuf::toString('maxRes_', $this->maxRes_);
    }
    
    // optional string wizard = 1;

    private $wizard_ = null;
    public function clearWizard() { $this->wizard_ = null; }
    public function hasWizard() { return $this->wizard_ !== null; }
    public function getWizard() { if($this->wizard_ === null) return ""; else return $this->wizard_; }
    public function setWizard($value) { $this->wizard_ = $value; }
    
    // optional int32 max_res = 2;

    private $maxRes_ = null;
    public function clearMaxRes() { $this->maxRes_ = null; }
    public function hasMaxRes() { return $this->maxRes_ !== null; }
    public function getMaxRes() { if($this->maxRes_ === null) return 0; else return $this->maxRes_; }
    public function setMaxRes($value) { $this->maxRes_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.LastModifications)
  }
  
  // message geneweb.api.object.LastVisits
  class LastVisits {
    private $_unknown;
    
    function __construct($in = NULL, &$limit = PHP_INT_MAX) {
      if($in !== NULL) {
        if (is_string($in)) {
          $fp = fopen('php://memory', 'r+b');
          fwrite($fp, $in);
          rewind($fp);
        } else if (is_resource($in)) {
          $fp = $in;
        } else {
          throw new \Exception('Invalid in parameter');
        }
        $this->read($fp, $limit);
      }
    }
    
    function read($fp, &$limit = PHP_INT_MAX) {
      while(!feof($fp) && $limit > 0) {
        $tag = \Protobuf::read_varint($fp, $limit);
        if ($tag === false) break;
        $wire  = $tag & 0x07;
        $field = $tag >> 3;
        //var_dump("\geneweb\api\object\LastVisits: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new \Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new \Exception("fread($len) returned false");
            $this->user_ = $tmp;
            $limit-=$len;
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
    }
    
    function write($fp) {
      if (!$this->validateRequired())
        throw new \Exception('Required fields are missing');
      if (!is_null($this->user_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, strlen($this->user_));
        fwrite($fp, $this->user_);
      }
    }
    
    public function size() {
      $size = 0;
      if (!is_null($this->user_)) {
        $l = strlen($this->user_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }
    
    public function validateRequired() {
      if ($this->user_ === null) return false;
      return true;
    }
    
    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('user_', $this->user_);
    }
    
    // required string user = 1;

    private $user_ = null;
    public function clearUser() { $this->user_ = null; }
    public function hasUser() { return $this->user_ !== null; }
    public function getUser() { if($this->user_ === null) return ""; else return $this->user_; }
    public function setUser($value) { $this->user_ = $value; }
    
    // @@protoc_insertion_point(class_scope:geneweb.api.object.LastVisits)
  }
  
  // enum geneweb.api.object.Sex
  class Sex {
    const MALE = 0;
    const FEMALE = 1;
    const UNKNOWN = 2;
    
    public static $_values = array(
      0 => self::MALE,
      1 => self::FEMALE,
      2 => self::UNKNOWN,
    );
    
    public static function toString($value) {
      if (is_null($value)) return null;
      if (array_key_exists($value, self::$_values))
        return self::$_values[$value];
      return 'UNKNOWN';
    }
  }
  
  // enum geneweb.api.object.DeathType
  class DeathType {
    const NOT_DEAD = 0;
    const DEAD = 1;
    const DEAD_YOUNG = 2;
    const DEAD_DONT_KNOW_WHEN = 3;
    const DONT_KNOW_IF_DEAD = 4;
    const OF_COURSE_DEAD = 5;
    
    public static $_values = array(
      0 => self::NOT_DEAD,
      1 => self::DEAD,
      2 => self::DEAD_YOUNG,
      3 => self::DEAD_DONT_KNOW_WHEN,
      4 => self::DONT_KNOW_IF_DEAD,
      5 => self::OF_COURSE_DEAD,
    );
    
    public static function toString($value) {
      if (is_null($value)) return null;
      if (array_key_exists($value, self::$_values))
        return self::$_values[$value];
      return 'UNKNOWN';
    }
  }
  
  // enum geneweb.api.object.MarriageType
  class MarriageType {
    const MARRIED = 0;
    const NOT_MARRIED = 1;
    const ENGAGED = 2;
    const NO_SEXES_CHECK_NOT_MARRIED = 3;
    const NO_MENTION = 4;
    const NO_SEXES_CHECK_MARRIED = 5;
    
    public static $_values = array(
      0 => self::MARRIED,
      1 => self::NOT_MARRIED,
      2 => self::ENGAGED,
      3 => self::NO_SEXES_CHECK_NOT_MARRIED,
      4 => self::NO_MENTION,
      5 => self::NO_SEXES_CHECK_MARRIED,
    );
    
    public static function toString($value) {
      if (is_null($value)) return null;
      if (array_key_exists($value, self::$_values))
        return self::$_values[$value];
      return 'UNKNOWN';
    }
  }
  
  // enum geneweb.api.object.DivorceType
  class DivorceType {
    const NOT_DIVORCED = 0;
    const DIVORCED = 1;
    const SEPARATED = 2;
    
    public static $_values = array(
      0 => self::NOT_DIVORCED,
      1 => self::DIVORCED,
      2 => self::SEPARATED,
    );
    
    public static function toString($value) {
      if (is_null($value)) return null;
      if (array_key_exists($value, self::$_values))
        return self::$_values[$value];
      return 'UNKNOWN';
    }
  }
  
  // enum geneweb.api.object.RelationParentType
  class RelationParentType {
    const RPT_ADOPTION = 0;
    const RPT_RECOGNITION = 1;
    const RPT_CANDIDATE_PARENT = 2;
    const RPT_GOD_PARENT = 3;
    const RPT_FOSTER_PARENT = 4;
    
    public static $_values = array(
      0 => self::RPT_ADOPTION,
      1 => self::RPT_RECOGNITION,
      2 => self::RPT_CANDIDATE_PARENT,
      3 => self::RPT_GOD_PARENT,
      4 => self::RPT_FOSTER_PARENT,
    );
    
    public static function toString($value) {
      if (is_null($value)) return null;
      if (array_key_exists($value, self::$_values))
        return self::$_values[$value];
      return 'UNKNOWN';
    }
  }
  
  // enum geneweb.api.object.TitleType
  class TitleType {
    const TITLE_MAIN = 0;
    const TITLE_NAME = 1;
    const TITLE_NONE = 2;
    
    public static $_values = array(
      0 => self::TITLE_MAIN,
      1 => self::TITLE_NAME,
      2 => self::TITLE_NONE,
    );
    
    public static function toString($value) {
      if (is_null($value)) return null;
      if (array_key_exists($value, self::$_values))
        return self::$_values[$value];
      return 'UNKNOWN';
    }
  }
  
  // enum geneweb.api.object.SearchType
  class SearchType {
    const STARTING_WITH = 0;
    const APPROXIMATIVE = 1;
    const LASTNAME_OR_FIRSTNAME = 2;
    
    public static $_values = array(
      0 => self::STARTING_WITH,
      1 => self::APPROXIMATIVE,
      2 => self::LASTNAME_OR_FIRSTNAME,
    );
    
    public static function toString($value) {
      if (is_null($value)) return null;
      if (array_key_exists($value, self::$_values))
        return self::$_values[$value];
      return 'UNKNOWN';
    }
  }
  
  // enum geneweb.api.object.RelationType
  class RelationType {
    const SELF = 0;
    const SPOUSE = 1;
    const SIBLING = 2;
    const STEP_BROTHER = 3;
    const PARENT = 4;
    const STEP_PARENT = 5;
    const GRAND_PARENT = 6;
    const UNCLE = 7;
    const UNCLE_SPOUSE = 8;
    const COUSIN = 9;
    const COUSIN_SPOUSE = 10;
    const CHILD = 11;
    const STEP_CHILD = 12;
    const GRAND_CHILD = 13;
    const GRAND_CHILD_SPOUSE = 14;
    const GREAT_GRAND_CHILD = 15;
    const GREAT_GRAND_CHILD_SPOUSE = 16;
    const CHILD_COUSIN = 17;
    const CHILD_COUSIN_SPOUSE = 18;
    const GRAND_CHILD_COUSIN = 19;
    const GRAND_CHILD_COUSIN_SPOUSE = 20;
    const GREAT_GRAND_CHILD_COUSIN = 21;
    const GREAT_GRAND_CHILD_COUSIN_SPOUSE = 22;
    const NEPHEW = 23;
    const NEPHEW_SPOUSE = 24;
    const NEPHEW_SPOUSE_SPOUSE = 25;
    const GRAND_NEPHEW = 26;
    const GRAND_NEPHEW_SPOUSE = 27;
    const GRAND_NEPHEW_SPOUSE_SPOUSE = 28;
    const GREAT_GRAND_NEPHEW = 29;
    const GREAT_GRAND_NEPHEW_SPOUSE = 30;
    const GREAT_GRAND_NEPHEW_SPOUSE_SPOUSE = 31;
    const ADOPTIVE_PARENT = 32;
    const ADOPTIVE_CHILD = 33;
    const RECOGNIZED_PARENT = 34;
    const RECOGNIZED_CHILD = 35;
    const CANDIDATE_PARENT = 36;
    const CANDIDATE_CHILD = 37;
    const GOD_PARENT = 38;
    const GOD_CHILD = 39;
    const FOSTER_PARENT = 40;
    const FOSTER_CHILD = 41;
    const WITNESS = 42;
    const NO_RELATION = 43;
    
    public static $_values = array(
      0 => self::SELF,
      1 => self::SPOUSE,
      2 => self::SIBLING,
      3 => self::STEP_BROTHER,
      4 => self::PARENT,
      5 => self::STEP_PARENT,
      6 => self::GRAND_PARENT,
      7 => self::UNCLE,
      8 => self::UNCLE_SPOUSE,
      9 => self::COUSIN,
      10 => self::COUSIN_SPOUSE,
      11 => self::CHILD,
      12 => self::STEP_CHILD,
      13 => self::GRAND_CHILD,
      14 => self::GRAND_CHILD_SPOUSE,
      15 => self::GREAT_GRAND_CHILD,
      16 => self::GREAT_GRAND_CHILD_SPOUSE,
      17 => self::CHILD_COUSIN,
      18 => self::CHILD_COUSIN_SPOUSE,
      19 => self::GRAND_CHILD_COUSIN,
      20 => self::GRAND_CHILD_COUSIN_SPOUSE,
      21 => self::GREAT_GRAND_CHILD_COUSIN,
      22 => self::GREAT_GRAND_CHILD_COUSIN_SPOUSE,
      23 => self::NEPHEW,
      24 => self::NEPHEW_SPOUSE,
      25 => self::NEPHEW_SPOUSE_SPOUSE,
      26 => self::GRAND_NEPHEW,
      27 => self::GRAND_NEPHEW_SPOUSE,
      28 => self::GRAND_NEPHEW_SPOUSE_SPOUSE,
      29 => self::GREAT_GRAND_NEPHEW,
      30 => self::GREAT_GRAND_NEPHEW_SPOUSE,
      31 => self::GREAT_GRAND_NEPHEW_SPOUSE_SPOUSE,
      32 => self::ADOPTIVE_PARENT,
      33 => self::ADOPTIVE_CHILD,
      34 => self::RECOGNIZED_PARENT,
      35 => self::RECOGNIZED_CHILD,
      36 => self::CANDIDATE_PARENT,
      37 => self::CANDIDATE_CHILD,
      38 => self::GOD_PARENT,
      39 => self::GOD_CHILD,
      40 => self::FOSTER_PARENT,
      41 => self::FOSTER_CHILD,
      42 => self::WITNESS,
      43 => self::NO_RELATION,
    );
    
    public static function toString($value) {
      if (is_null($value)) return null;
      if (array_key_exists($value, self::$_values))
        return self::$_values[$value];
      return 'UNKNOWN';
    }
  }
  
  // enum geneweb.api.object.NotifBirthdayParams
  class NotifBirthdayParams {
    const CLOSE_PERSON = 0;
    const DESCEND_GRAND_PARENT = 1;
    const DESCEND_GREAT_GRAND_PARENT = 2;
    
    public static $_values = array(
      0 => self::CLOSE_PERSON,
      1 => self::DESCEND_GRAND_PARENT,
      2 => self::DESCEND_GREAT_GRAND_PARENT,
    );
    
    public static function toString($value) {
      if (is_null($value)) return null;
      if (array_key_exists($value, self::$_values))
        return self::$_values[$value];
      return 'UNKNOWN';
    }
  }
  
}
