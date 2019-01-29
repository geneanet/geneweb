<?php
// Please include the below file before api_saisie_read.proto.php
//
namespace Geneanet\Bundle\GenewebBundle\Api\SaisieRead\Object {
    
require_once('protocolbuffers.inc.php');


    use ProtobufEnum;
    use ProtobufMessage;
    use ProtoBuf;
    use Exception;

  // message Dmy
  class Dmy {
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
          throw new Exception('Invalid in parameter');
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
        //var_dump("Dmy: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->day_ = $tmp;

            break;
          case 2:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->month_ = $tmp;

            break;
          case 3:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->year_ = $tmp;

            break;
          case 4:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->delta_ = $tmp;

            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
    }

    function write($fp) {
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
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
      if (!is_null($this->delta_)) {
        fwrite($fp, " ");
        \Protobuf::write_varint($fp, $this->delta_);
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
      if (!is_null($this->delta_)) {
        $size += 1 + \Protobuf::size_varint($this->delta_);
      }
      return $size;
    }

    public function validateRequired() {
      if ($this->day_ === null) return false;
      if ($this->month_ === null) return false;
      if ($this->year_ === null) return false;
      if ($this->delta_ === null) return false;
      return true;
    }

    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('day_', $this->day_)
           . \Protobuf::toString('month_', $this->month_)
           . \Protobuf::toString('year_', $this->year_)
           . \Protobuf::toString('delta_', $this->delta_);
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

    // required int32 delta = 4;

    private $delta_ = null;
    public function clearDelta() { $this->delta_ = null; }
    public function hasDelta() { return $this->delta_ !== null; }
    public function getDelta() { if($this->delta_ === null) return 0; else return $this->delta_; }
    public function setDelta($value) { $this->delta_ = $value; }

    // @@protoc_insertion_point(class_scope:Dmy)
  }

  // message Date
  class Date {
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
          throw new Exception('Invalid in parameter');
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
        //var_dump("Date: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 2:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->cal_ = $tmp;

            break;
          case 3:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->prec_ = $tmp;

            break;
          case 4:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->dmy_ = new Dmy($fp, $len);
            ASSERT('$len == 0');
            break;
          case 5:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->dmy2_ = new Dmy($fp, $len);
            ASSERT('$len == 0');
            break;
          case 6:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->text_ = $tmp;
            $limit-=$len;
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
    }

    function write($fp) {
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
      if (!is_null($this->cal_)) {
        fwrite($fp, "\x10");
        \Protobuf::write_varint($fp, $this->cal_);
      }
      if (!is_null($this->prec_)) {
        fwrite($fp, "\x18");
        \Protobuf::write_varint($fp, $this->prec_);
      }
      if (!is_null($this->dmy_)) {
        fwrite($fp, "\"");
        \Protobuf::write_varint($fp, $this->dmy_->size()); // message
        $this->dmy_->write($fp);
      }
      if (!is_null($this->dmy2_)) {
        fwrite($fp, "*");
        \Protobuf::write_varint($fp, $this->dmy2_->size()); // message
        $this->dmy2_->write($fp);
      }
      if (!is_null($this->text_)) {
        fwrite($fp, "2");
        \Protobuf::write_varint($fp, strlen($this->text_));
        fwrite($fp, $this->text_);
      }
    }

    public function size() {
      $size = 0;
      if (!is_null($this->cal_)) {
        $size += 1 + \Protobuf::size_varint($this->cal_);
      }
      if (!is_null($this->prec_)) {
        $size += 1 + \Protobuf::size_varint($this->prec_);
      }
      if (!is_null($this->dmy_)) {
        $l = $this->dmy_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->dmy2_)) {
        $l = $this->dmy2_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->text_)) {
        $l = strlen($this->text_);
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
           . \Protobuf::toString('cal_', Calendar::toString($this->cal_))
           . \Protobuf::toString('prec_', Precision::toString($this->prec_))
           . \Protobuf::toString('dmy_', $this->dmy_)
           . \Protobuf::toString('dmy2_', $this->dmy2_)
           . \Protobuf::toString('text_', $this->text_);
    }

    // optional .Calendar cal = 2;

    private $cal_ = null;
    public function clearCal() { $this->cal_ = null; }
    public function hasCal() { return $this->cal_ !== null; }
    public function getCal() { if($this->cal_ === null) return Calendar::GREGORIAN; else return $this->cal_; }
    public function setCal($value) { $this->cal_ = $value; }

    // optional .Precision prec = 3;

    private $prec_ = null;
    public function clearPrec() { $this->prec_ = null; }
    public function hasPrec() { return $this->prec_ !== null; }
    public function getPrec() { if($this->prec_ === null) return Precision::SURE; else return $this->prec_; }
    public function setPrec($value) { $this->prec_ = $value; }

    // optional .Dmy dmy = 4;

    private $dmy_ = null;
    public function clearDmy() { $this->dmy_ = null; }
    public function hasDmy() { return $this->dmy_ !== null; }
    public function getDmy() { if($this->dmy_ === null) return null; else return $this->dmy_; }
    public function setDmy(Dmy $value) { $this->dmy_ = $value; }

    // optional .Dmy dmy2 = 5;

    private $dmy2_ = null;
    public function clearDmy2() { $this->dmy2_ = null; }
    public function hasDmy2() { return $this->dmy2_ !== null; }
    public function getDmy2() { if($this->dmy2_ === null) return null; else return $this->dmy2_; }
    public function setDmy2(Dmy $value) { $this->dmy2_ = $value; }

    // optional string text = 6;

    private $text_ = null;
    public function clearText() { $this->text_ = null; }
    public function hasText() { return $this->text_ !== null; }
    public function getText() { if($this->text_ === null) return ""; else return $this->text_; }
    public function setText($value) { $this->text_ = $value; }

    // @@protoc_insertion_point(class_scope:Date)
  }

  // message WitnessEvent
  class WitnessEvent {
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
          throw new Exception('Invalid in parameter');
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
        //var_dump("WitnessEvent: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->witnessType_ = $tmp;

            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->witness_ = new SimplePerson($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
    }

    function write($fp) {
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
      if (!is_null($this->witnessType_)) {
        fwrite($fp, "\x08");
        \Protobuf::write_varint($fp, $this->witnessType_);
      }
      if (!is_null($this->witness_)) {
        fwrite($fp, "\x12");
        \Protobuf::write_varint($fp, $this->witness_->size()); // message
        $this->witness_->write($fp);
      }
    }

    public function size() {
      $size = 0;
      if (!is_null($this->witnessType_)) {
        $size += 1 + \Protobuf::size_varint($this->witnessType_);
      }
      if (!is_null($this->witness_)) {
        $l = $this->witness_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }

    public function validateRequired() {
      if ($this->witnessType_ === null) return false;
      if ($this->witness_ === null) return false;
      return true;
    }

    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('witnessType_', WitnessType::toString($this->witnessType_))
           . \Protobuf::toString('witness_', $this->witness_);
    }

    // required .WitnessType witness_type = 1;

    private $witnessType_ = null;
    public function clearWitnessType() { $this->witnessType_ = null; }
    public function hasWitnessType() { return $this->witnessType_ !== null; }
    public function getWitnessType() { if($this->witnessType_ === null) return WitnessType::WITNESS; else return $this->witnessType_; }
    public function setWitnessType($value) { $this->witnessType_ = $value; }

    // required .SimplePerson witness = 2;

    private $witness_ = null;
    public function clearWitness() { $this->witness_ = null; }
    public function hasWitness() { return $this->witness_ !== null; }
    public function getWitness() { if($this->witness_ === null) return null; else return $this->witness_; }
    public function setWitness(SimplePerson $value) { $this->witness_ = $value; }

    // @@protoc_insertion_point(class_scope:WitnessEvent)
  }

  // message Event
  class Event {
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
          throw new Exception('Invalid in parameter');
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
        //var_dump("Event: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->name_ = $tmp;
            $limit-=$len;
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->date_ = $tmp;
            $limit-=$len;
            break;
          case 3:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->dateConv_ = $tmp;
            $limit-=$len;
            break;
          case 4:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->dateCal_ = $tmp;

            break;
          case 5:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->place_ = $tmp;
            $limit-=$len;
            break;
          case 6:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->reason_ = $tmp;
            $limit-=$len;
            break;
          case 7:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->note_ = $tmp;
            $limit-=$len;
            break;
          case 8:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->src_ = $tmp;
            $limit-=$len;
            break;
          case 9:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->spouse_ = new SimplePerson($fp, $len);
            ASSERT('$len == 0');
            break;
          case 10:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->witnesses_[] = new WitnessEvent($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
    }

    function write($fp) {
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
      if (!is_null($this->name_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, strlen($this->name_));
        fwrite($fp, $this->name_);
      }
      if (!is_null($this->date_)) {
        fwrite($fp, "\x12");
        \Protobuf::write_varint($fp, strlen($this->date_));
        fwrite($fp, $this->date_);
      }
      if (!is_null($this->dateConv_)) {
        fwrite($fp, "\x1a");
        \Protobuf::write_varint($fp, strlen($this->dateConv_));
        fwrite($fp, $this->dateConv_);
      }
      if (!is_null($this->dateCal_)) {
        fwrite($fp, " ");
        \Protobuf::write_varint($fp, $this->dateCal_);
      }
      if (!is_null($this->place_)) {
        fwrite($fp, "*");
        \Protobuf::write_varint($fp, strlen($this->place_));
        fwrite($fp, $this->place_);
      }
      if (!is_null($this->reason_)) {
        fwrite($fp, "2");
        \Protobuf::write_varint($fp, strlen($this->reason_));
        fwrite($fp, $this->reason_);
      }
      if (!is_null($this->note_)) {
        fwrite($fp, ":");
        \Protobuf::write_varint($fp, strlen($this->note_));
        fwrite($fp, $this->note_);
      }
      if (!is_null($this->src_)) {
        fwrite($fp, "B");
        \Protobuf::write_varint($fp, strlen($this->src_));
        fwrite($fp, $this->src_);
      }
      if (!is_null($this->spouse_)) {
        fwrite($fp, "J");
        \Protobuf::write_varint($fp, $this->spouse_->size()); // message
        $this->spouse_->write($fp);
      }
      if (!is_null($this->witnesses_))
        foreach($this->witnesses_ as $v) {
          fwrite($fp, "R");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
    }

    public function size() {
      $size = 0;
      if (!is_null($this->name_)) {
        $l = strlen($this->name_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->date_)) {
        $l = strlen($this->date_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->dateConv_)) {
        $l = strlen($this->dateConv_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->dateCal_)) {
        $size += 1 + \Protobuf::size_varint($this->dateCal_);
      }
      if (!is_null($this->place_)) {
        $l = strlen($this->place_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->reason_)) {
        $l = strlen($this->reason_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->note_)) {
        $l = strlen($this->note_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->src_)) {
        $l = strlen($this->src_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->spouse_)) {
        $l = $this->spouse_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->witnesses_))
        foreach($this->witnesses_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      return $size;
    }

    public function validateRequired() {
      if ($this->name_ === null) return false;
      return true;
    }

    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('name_', $this->name_)
           . \Protobuf::toString('date_', $this->date_)
           . \Protobuf::toString('dateConv_', $this->dateConv_)
           . \Protobuf::toString('dateCal_', Calendar::toString($this->dateCal_))
           . \Protobuf::toString('place_', $this->place_)
           . \Protobuf::toString('reason_', $this->reason_)
           . \Protobuf::toString('note_', $this->note_)
           . \Protobuf::toString('src_', $this->src_)
           . \Protobuf::toString('spouse_', $this->spouse_)
           . \Protobuf::toString('witnesses_', $this->witnesses_);
    }

    // required string name = 1;

    private $name_ = null;
    public function clearName() { $this->name_ = null; }
    public function hasName() { return $this->name_ !== null; }
    public function getName() { if($this->name_ === null) return ""; else return $this->name_; }
    public function setName($value) { $this->name_ = $value; }

    // optional string date = 2;

    private $date_ = null;
    public function clearDate() { $this->date_ = null; }
    public function hasDate() { return $this->date_ !== null; }
    public function getDate() { if($this->date_ === null) return ""; else return $this->date_; }
    public function setDate($value) { $this->date_ = $value; }

    // optional string date_conv = 3;

    private $dateConv_ = null;
    public function clearDateConv() { $this->dateConv_ = null; }
    public function hasDateConv() { return $this->dateConv_ !== null; }
    public function getDateConv() { if($this->dateConv_ === null) return ""; else return $this->dateConv_; }
    public function setDateConv($value) { $this->dateConv_ = $value; }

    // optional .Calendar date_cal = 4;

    private $dateCal_ = null;
    public function clearDateCal() { $this->dateCal_ = null; }
    public function hasDateCal() { return $this->dateCal_ !== null; }
    public function getDateCal() { if($this->dateCal_ === null) return Calendar::GREGORIAN; else return $this->dateCal_; }
    public function setDateCal($value) { $this->dateCal_ = $value; }

    // optional string place = 5;

    private $place_ = null;
    public function clearPlace() { $this->place_ = null; }
    public function hasPlace() { return $this->place_ !== null; }
    public function getPlace() { if($this->place_ === null) return ""; else return $this->place_; }
    public function setPlace($value) { $this->place_ = $value; }

    // optional string reason = 6;

    private $reason_ = null;
    public function clearReason() { $this->reason_ = null; }
    public function hasReason() { return $this->reason_ !== null; }
    public function getReason() { if($this->reason_ === null) return ""; else return $this->reason_; }
    public function setReason($value) { $this->reason_ = $value; }

    // optional string note = 7;

    private $note_ = null;
    public function clearNote() { $this->note_ = null; }
    public function hasNote() { return $this->note_ !== null; }
    public function getNote() { if($this->note_ === null) return ""; else return $this->note_; }
    public function setNote($value) { $this->note_ = $value; }

    // optional string src = 8;

    private $src_ = null;
    public function clearSrc() { $this->src_ = null; }
    public function hasSrc() { return $this->src_ !== null; }
    public function getSrc() { if($this->src_ === null) return ""; else return $this->src_; }
    public function setSrc($value) { $this->src_ = $value; }

    // optional .SimplePerson spouse = 9;

    private $spouse_ = null;
    public function clearSpouse() { $this->spouse_ = null; }
    public function hasSpouse() { return $this->spouse_ !== null; }
    public function getSpouse() { if($this->spouse_ === null) return null; else return $this->spouse_; }
    public function setSpouse(SimplePerson $value) { $this->spouse_ = $value; }

    // repeated .WitnessEvent witnesses = 10;

    private $witnesses_ = null;
    public function clearWitnesses() { $this->witnesses_ = null; }
    public function getWitnessesCount() { if ($this->witnesses_ === null ) return 0; else return count($this->witnesses_); }
    public function getWitnesses($index) { return $this->witnesses_[$index]; }
    public function getWitnessesArray() { if ($this->witnesses_ === null ) return array(); else return $this->witnesses_; }
    public function setWitnesses($index, $value) {$this->witnesses_[$index] = $value;	}
    public function addWitnesses($value) { $this->witnesses_[] = $value; }
    public function addAllWitnesses(array $values) { foreach($values as $value) {$this->witnesses_[] = $value;} }

    // @@protoc_insertion_point(class_scope:Event)
  }

  // message PersonTree
  class PersonTree {
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
          throw new Exception('Invalid in parameter');
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
        //var_dump("PersonTree: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->index_ = $tmp;

            break;
          case 2:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->sex_ = $tmp;

            break;
          case 3:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->lastname_ = $tmp;
            $limit-=$len;
            break;
          case 4:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->firstname_ = $tmp;
            $limit-=$len;
            break;
          case 5:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->n_ = $tmp;
            $limit-=$len;
            break;
          case 6:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->p_ = $tmp;
            $limit-=$len;
            break;
          case 7:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->occ_ = $tmp;

            break;
          case 8:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->dates_ = $tmp;
            $limit-=$len;
            break;
          case 9:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->image_ = $tmp;
            $limit-=$len;
            break;
          case 10:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->sosa_ = $tmp;

            break;
          case 11:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->hasMoreInfos_ = $tmp > 0 ? true : false;
            break;
          case 12:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->baseprefix_ = $tmp;
            $limit-=$len;
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
    }

    function write($fp) {
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
      if (!is_null($this->index_)) {
        fwrite($fp, "\x08");
        \Protobuf::write_varint($fp, $this->index_);
      }
      if (!is_null($this->sex_)) {
        fwrite($fp, "\x10");
        \Protobuf::write_varint($fp, $this->sex_);
      }
      if (!is_null($this->lastname_)) {
        fwrite($fp, "\x1a");
        \Protobuf::write_varint($fp, strlen($this->lastname_));
        fwrite($fp, $this->lastname_);
      }
      if (!is_null($this->firstname_)) {
        fwrite($fp, "\"");
        \Protobuf::write_varint($fp, strlen($this->firstname_));
        fwrite($fp, $this->firstname_);
      }
      if (!is_null($this->n_)) {
        fwrite($fp, "*");
        \Protobuf::write_varint($fp, strlen($this->n_));
        fwrite($fp, $this->n_);
      }
      if (!is_null($this->p_)) {
        fwrite($fp, "2");
        \Protobuf::write_varint($fp, strlen($this->p_));
        fwrite($fp, $this->p_);
      }
      if (!is_null($this->occ_)) {
        fwrite($fp, "8");
        \Protobuf::write_varint($fp, $this->occ_);
      }
      if (!is_null($this->dates_)) {
        fwrite($fp, "B");
        \Protobuf::write_varint($fp, strlen($this->dates_));
        fwrite($fp, $this->dates_);
      }
      if (!is_null($this->image_)) {
        fwrite($fp, "J");
        \Protobuf::write_varint($fp, strlen($this->image_));
        fwrite($fp, $this->image_);
      }
      if (!is_null($this->sosa_)) {
        fwrite($fp, "P");
        \Protobuf::write_varint($fp, $this->sosa_);
      }
      if (!is_null($this->hasMoreInfos_)) {
        fwrite($fp, "X");
        \Protobuf::write_varint($fp, $this->hasMoreInfos_ ? 1 : 0);
      }
      if (!is_null($this->baseprefix_)) {
        fwrite($fp, "b");
        \Protobuf::write_varint($fp, strlen($this->baseprefix_));
        fwrite($fp, $this->baseprefix_);
      }
    }

    public function size() {
      $size = 0;
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
      if (!is_null($this->n_)) {
        $l = strlen($this->n_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->p_)) {
        $l = strlen($this->p_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->occ_)) {
        $size += 1 + \Protobuf::size_varint($this->occ_);
      }
      if (!is_null($this->dates_)) {
        $l = strlen($this->dates_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->image_)) {
        $l = strlen($this->image_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->sosa_)) {
        $size += 1 + \Protobuf::size_varint($this->sosa_);
      }
      if (!is_null($this->hasMoreInfos_)) {
        $size += 2;
      }
      if (!is_null($this->baseprefix_)) {
        $l = strlen($this->baseprefix_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }

    public function validateRequired() {
      if ($this->index_ === null) return false;
      if ($this->sex_ === null) return false;
      if ($this->lastname_ === null) return false;
      if ($this->firstname_ === null) return false;
      if ($this->n_ === null) return false;
      if ($this->p_ === null) return false;
      if ($this->occ_ === null) return false;
      if ($this->sosa_ === null) return false;
      if ($this->hasMoreInfos_ === null) return false;
      if ($this->baseprefix_ === null) return false;
      return true;
    }

    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('index_', $this->index_)
           . \Protobuf::toString('sex_', Sex::toString($this->sex_))
           . \Protobuf::toString('lastname_', $this->lastname_)
           . \Protobuf::toString('firstname_', $this->firstname_)
           . \Protobuf::toString('n_', $this->n_)
           . \Protobuf::toString('p_', $this->p_)
           . \Protobuf::toString('occ_', $this->occ_)
           . \Protobuf::toString('dates_', $this->dates_)
           . \Protobuf::toString('image_', $this->image_)
           . \Protobuf::toString('sosa_', Sosa::toString($this->sosa_))
           . \Protobuf::toString('hasMoreInfos_', $this->hasMoreInfos_)
           . \Protobuf::toString('baseprefix_', $this->baseprefix_);
    }

    // required int32 index = 1;

    private $index_ = null;
    public function clearIndex() { $this->index_ = null; }
    public function hasIndex() { return $this->index_ !== null; }
    public function getIndex() { if($this->index_ === null) return 0; else return $this->index_; }
    public function setIndex($value) { $this->index_ = $value; }

    // required .Sex sex = 2;

    private $sex_ = null;
    public function clearSex() { $this->sex_ = null; }
    public function hasSex() { return $this->sex_ !== null; }
    public function getSex() { if($this->sex_ === null) return Sex::MALE; else return $this->sex_; }
    public function setSex($value) { $this->sex_ = $value; }

    // required string lastname = 3;

    private $lastname_ = null;
    public function clearLastname() { $this->lastname_ = null; }
    public function hasLastname() { return $this->lastname_ !== null; }
    public function getLastname() { if($this->lastname_ === null) return ""; else return $this->lastname_; }
    public function setLastname($value) { $this->lastname_ = $value; }

    // required string firstname = 4;

    private $firstname_ = null;
    public function clearFirstname() { $this->firstname_ = null; }
    public function hasFirstname() { return $this->firstname_ !== null; }
    public function getFirstname() { if($this->firstname_ === null) return ""; else return $this->firstname_; }
    public function setFirstname($value) { $this->firstname_ = $value; }

    // required string n = 5;

    private $n_ = null;
    public function clearN() { $this->n_ = null; }
    public function hasN() { return $this->n_ !== null; }
    public function getN() { if($this->n_ === null) return ""; else return $this->n_; }
    public function setN($value) { $this->n_ = $value; }

    // required string p = 6;

    private $p_ = null;
    public function clearP() { $this->p_ = null; }
    public function hasP() { return $this->p_ !== null; }
    public function getP() { if($this->p_ === null) return ""; else return $this->p_; }
    public function setP($value) { $this->p_ = $value; }

    // required int32 occ = 7;

    private $occ_ = null;
    public function clearOcc() { $this->occ_ = null; }
    public function hasOcc() { return $this->occ_ !== null; }
    public function getOcc() { if($this->occ_ === null) return 0; else return $this->occ_; }
    public function setOcc($value) { $this->occ_ = $value; }

    // optional string dates = 8;

    private $dates_ = null;
    public function clearDates() { $this->dates_ = null; }
    public function hasDates() { return $this->dates_ !== null; }
    public function getDates() { if($this->dates_ === null) return ""; else return $this->dates_; }
    public function setDates($value) { $this->dates_ = $value; }

    // optional string image = 9;

    private $image_ = null;
    public function clearImage() { $this->image_ = null; }
    public function hasImage() { return $this->image_ !== null; }
    public function getImage() { if($this->image_ === null) return ""; else return $this->image_; }
    public function setImage($value) { $this->image_ = $value; }

    // required .Sosa sosa = 10;

    private $sosa_ = null;
    public function clearSosa() { $this->sosa_ = null; }
    public function hasSosa() { return $this->sosa_ !== null; }
    public function getSosa() { if($this->sosa_ === null) return Sosa::SOSA_REF; else return $this->sosa_; }
    public function setSosa($value) { $this->sosa_ = $value; }

    // required bool has_more_infos = 11;

    private $hasMoreInfos_ = null;
    public function clearHasMoreInfos() { $this->hasMoreInfos_ = null; }
    public function hasHasMoreInfos() { return $this->hasMoreInfos_ !== null; }
    public function getHasMoreInfos() { if($this->hasMoreInfos_ === null) return false; else return $this->hasMoreInfos_; }
    public function setHasMoreInfos($value) { $this->hasMoreInfos_ = $value; }

    // required string baseprefix = 12;

    private $baseprefix_ = null;
    public function clearBaseprefix() { $this->baseprefix_ = null; }
    public function hasBaseprefix() { return $this->baseprefix_ !== null; }
    public function getBaseprefix() { if($this->baseprefix_ === null) return ""; else return $this->baseprefix_; }
    public function setBaseprefix($value) { $this->baseprefix_ = $value; }

    // @@protoc_insertion_point(class_scope:PersonTree)
  }

  // message SimplePerson
  class SimplePerson {
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
          throw new Exception('Invalid in parameter');
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
        //var_dump("SimplePerson: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->index_ = $tmp;

            break;
          case 2:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->sex_ = $tmp;

            break;
          case 3:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->lastname_ = $tmp;
            $limit-=$len;
            break;
          case 4:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->firstname_ = $tmp;
            $limit-=$len;
            break;
          case 5:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->n_ = $tmp;
            $limit-=$len;
            break;
          case 6:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->p_ = $tmp;
            $limit-=$len;
            break;
          case 7:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->occ_ = $tmp;

            break;
          case 8:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->birthShortDate_ = $tmp;
            $limit-=$len;
            break;
          case 9:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->birthPlace_ = $tmp;
            $limit-=$len;
            break;
          case 10:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->deathShortDate_ = $tmp;
            $limit-=$len;
            break;
          case 11:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->deathPlace_ = $tmp;
            $limit-=$len;
            break;
          case 12:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->image_ = $tmp;
            $limit-=$len;
            break;
          case 13:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->sosa_ = $tmp;

            break;
          case 14:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->baseprefix_ = $tmp;
            $limit-=$len;
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
    }

    function write($fp) {
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
      if (!is_null($this->index_)) {
        fwrite($fp, "\x08");
        \Protobuf::write_varint($fp, $this->index_);
      }
      if (!is_null($this->sex_)) {
        fwrite($fp, "\x10");
        \Protobuf::write_varint($fp, $this->sex_);
      }
      if (!is_null($this->lastname_)) {
        fwrite($fp, "\x1a");
        \Protobuf::write_varint($fp, strlen($this->lastname_));
        fwrite($fp, $this->lastname_);
      }
      if (!is_null($this->firstname_)) {
        fwrite($fp, "\"");
        \Protobuf::write_varint($fp, strlen($this->firstname_));
        fwrite($fp, $this->firstname_);
      }
      if (!is_null($this->n_)) {
        fwrite($fp, "*");
        \Protobuf::write_varint($fp, strlen($this->n_));
        fwrite($fp, $this->n_);
      }
      if (!is_null($this->p_)) {
        fwrite($fp, "2");
        \Protobuf::write_varint($fp, strlen($this->p_));
        fwrite($fp, $this->p_);
      }
      if (!is_null($this->occ_)) {
        fwrite($fp, "8");
        \Protobuf::write_varint($fp, $this->occ_);
      }
      if (!is_null($this->birthShortDate_)) {
        fwrite($fp, "B");
        \Protobuf::write_varint($fp, strlen($this->birthShortDate_));
        fwrite($fp, $this->birthShortDate_);
      }
      if (!is_null($this->birthPlace_)) {
        fwrite($fp, "J");
        \Protobuf::write_varint($fp, strlen($this->birthPlace_));
        fwrite($fp, $this->birthPlace_);
      }
      if (!is_null($this->deathShortDate_)) {
        fwrite($fp, "R");
        \Protobuf::write_varint($fp, strlen($this->deathShortDate_));
        fwrite($fp, $this->deathShortDate_);
      }
      if (!is_null($this->deathPlace_)) {
        fwrite($fp, "Z");
        \Protobuf::write_varint($fp, strlen($this->deathPlace_));
        fwrite($fp, $this->deathPlace_);
      }
      if (!is_null($this->image_)) {
        fwrite($fp, "b");
        \Protobuf::write_varint($fp, strlen($this->image_));
        fwrite($fp, $this->image_);
      }
      if (!is_null($this->sosa_)) {
        fwrite($fp, "h");
        \Protobuf::write_varint($fp, $this->sosa_);
      }
      if (!is_null($this->baseprefix_)) {
        fwrite($fp, "r");
        \Protobuf::write_varint($fp, strlen($this->baseprefix_));
        fwrite($fp, $this->baseprefix_);
      }
    }

    public function size() {
      $size = 0;
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
      if (!is_null($this->n_)) {
        $l = strlen($this->n_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->p_)) {
        $l = strlen($this->p_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->occ_)) {
        $size += 1 + \Protobuf::size_varint($this->occ_);
      }
      if (!is_null($this->birthShortDate_)) {
        $l = strlen($this->birthShortDate_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->birthPlace_)) {
        $l = strlen($this->birthPlace_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->deathShortDate_)) {
        $l = strlen($this->deathShortDate_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->deathPlace_)) {
        $l = strlen($this->deathPlace_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->image_)) {
        $l = strlen($this->image_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->sosa_)) {
        $size += 1 + \Protobuf::size_varint($this->sosa_);
      }
      if (!is_null($this->baseprefix_)) {
        $l = strlen($this->baseprefix_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }

    public function validateRequired() {
      if ($this->index_ === null) return false;
      if ($this->sex_ === null) return false;
      if ($this->lastname_ === null) return false;
      if ($this->firstname_ === null) return false;
      if ($this->n_ === null) return false;
      if ($this->p_ === null) return false;
      if ($this->occ_ === null) return false;
      if ($this->sosa_ === null) return false;
      if ($this->baseprefix_ === null) return false;
      return true;
    }

    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('index_', $this->index_)
           . \Protobuf::toString('sex_', Sex::toString($this->sex_))
           . \Protobuf::toString('lastname_', $this->lastname_)
           . \Protobuf::toString('firstname_', $this->firstname_)
           . \Protobuf::toString('n_', $this->n_)
           . \Protobuf::toString('p_', $this->p_)
           . \Protobuf::toString('occ_', $this->occ_)
           . \Protobuf::toString('birthShortDate_', $this->birthShortDate_)
           . \Protobuf::toString('birthPlace_', $this->birthPlace_)
           . \Protobuf::toString('deathShortDate_', $this->deathShortDate_)
           . \Protobuf::toString('deathPlace_', $this->deathPlace_)
           . \Protobuf::toString('image_', $this->image_)
           . \Protobuf::toString('sosa_', Sosa::toString($this->sosa_))
           . \Protobuf::toString('baseprefix_', $this->baseprefix_);
    }

    // required int32 index = 1;

    private $index_ = null;
    public function clearIndex() { $this->index_ = null; }
    public function hasIndex() { return $this->index_ !== null; }
    public function getIndex() { if($this->index_ === null) return 0; else return $this->index_; }
    public function setIndex($value) { $this->index_ = $value; }

    // required .Sex sex = 2;

    private $sex_ = null;
    public function clearSex() { $this->sex_ = null; }
    public function hasSex() { return $this->sex_ !== null; }
    public function getSex() { if($this->sex_ === null) return Sex::MALE; else return $this->sex_; }
    public function setSex($value) { $this->sex_ = $value; }

    // required string lastname = 3;

    private $lastname_ = null;
    public function clearLastname() { $this->lastname_ = null; }
    public function hasLastname() { return $this->lastname_ !== null; }
    public function getLastname() { if($this->lastname_ === null) return ""; else return $this->lastname_; }
    public function setLastname($value) { $this->lastname_ = $value; }

    // required string firstname = 4;

    private $firstname_ = null;
    public function clearFirstname() { $this->firstname_ = null; }
    public function hasFirstname() { return $this->firstname_ !== null; }
    public function getFirstname() { if($this->firstname_ === null) return ""; else return $this->firstname_; }
    public function setFirstname($value) { $this->firstname_ = $value; }

    // required string n = 5;

    private $n_ = null;
    public function clearN() { $this->n_ = null; }
    public function hasN() { return $this->n_ !== null; }
    public function getN() { if($this->n_ === null) return ""; else return $this->n_; }
    public function setN($value) { $this->n_ = $value; }

    // required string p = 6;

    private $p_ = null;
    public function clearP() { $this->p_ = null; }
    public function hasP() { return $this->p_ !== null; }
    public function getP() { if($this->p_ === null) return ""; else return $this->p_; }
    public function setP($value) { $this->p_ = $value; }

    // required int32 occ = 7;

    private $occ_ = null;
    public function clearOcc() { $this->occ_ = null; }
    public function hasOcc() { return $this->occ_ !== null; }
    public function getOcc() { if($this->occ_ === null) return 0; else return $this->occ_; }
    public function setOcc($value) { $this->occ_ = $value; }

    // optional string birth_short_date = 8;

    private $birthShortDate_ = null;
    public function clearBirthShortDate() { $this->birthShortDate_ = null; }
    public function hasBirthShortDate() { return $this->birthShortDate_ !== null; }
    public function getBirthShortDate() { if($this->birthShortDate_ === null) return ""; else return $this->birthShortDate_; }
    public function setBirthShortDate($value) { $this->birthShortDate_ = $value; }

    // optional string birth_place = 9;

    private $birthPlace_ = null;
    public function clearBirthPlace() { $this->birthPlace_ = null; }
    public function hasBirthPlace() { return $this->birthPlace_ !== null; }
    public function getBirthPlace() { if($this->birthPlace_ === null) return ""; else return $this->birthPlace_; }
    public function setBirthPlace($value) { $this->birthPlace_ = $value; }

    // optional string death_short_date = 10;

    private $deathShortDate_ = null;
    public function clearDeathShortDate() { $this->deathShortDate_ = null; }
    public function hasDeathShortDate() { return $this->deathShortDate_ !== null; }
    public function getDeathShortDate() { if($this->deathShortDate_ === null) return ""; else return $this->deathShortDate_; }
    public function setDeathShortDate($value) { $this->deathShortDate_ = $value; }

    // optional string death_place = 11;

    private $deathPlace_ = null;
    public function clearDeathPlace() { $this->deathPlace_ = null; }
    public function hasDeathPlace() { return $this->deathPlace_ !== null; }
    public function getDeathPlace() { if($this->deathPlace_ === null) return ""; else return $this->deathPlace_; }
    public function setDeathPlace($value) { $this->deathPlace_ = $value; }

    // optional string image = 12;

    private $image_ = null;
    public function clearImage() { $this->image_ = null; }
    public function hasImage() { return $this->image_ !== null; }
    public function getImage() { if($this->image_ === null) return ""; else return $this->image_; }
    public function setImage($value) { $this->image_ = $value; }

    // required .Sosa sosa = 13;

    private $sosa_ = null;
    public function clearSosa() { $this->sosa_ = null; }
    public function hasSosa() { return $this->sosa_ !== null; }
    public function getSosa() { if($this->sosa_ === null) return Sosa::SOSA_REF; else return $this->sosa_; }
    public function setSosa($value) { $this->sosa_ = $value; }

    // required string baseprefix = 14;

    private $baseprefix_ = null;
    public function clearBaseprefix() { $this->baseprefix_ = null; }
    public function hasBaseprefix() { return $this->baseprefix_ !== null; }
    public function getBaseprefix() { if($this->baseprefix_ === null) return ""; else return $this->baseprefix_; }
    public function setBaseprefix($value) { $this->baseprefix_ = $value; }

    // @@protoc_insertion_point(class_scope:SimplePerson)
  }

  // message RelationPerson
  class RelationPerson {
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
          throw new Exception('Invalid in parameter');
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
        //var_dump("RelationPerson: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->rType_ = $tmp;

            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->person_ = new SimplePerson($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
    }

    function write($fp) {
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
      if (!is_null($this->rType_)) {
        fwrite($fp, "\x08");
        \Protobuf::write_varint($fp, $this->rType_);
      }
      if (!is_null($this->person_)) {
        fwrite($fp, "\x12");
        \Protobuf::write_varint($fp, $this->person_->size()); // message
        $this->person_->write($fp);
      }
    }

    public function size() {
      $size = 0;
      if (!is_null($this->rType_)) {
        $size += 1 + \Protobuf::size_varint($this->rType_);
      }
      if (!is_null($this->person_)) {
        $l = $this->person_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }

    public function validateRequired() {
      if ($this->rType_ === null) return false;
      if ($this->person_ === null) return false;
      return true;
    }

    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('rType_', RelationType::toString($this->rType_))
           . \Protobuf::toString('person_', $this->person_);
    }

    // required .RelationType r_type = 1;

    private $rType_ = null;
    public function clearRType() { $this->rType_ = null; }
    public function hasRType() { return $this->rType_ !== null; }
    public function getRType() { if($this->rType_ === null) return RelationType::RPARENT_ADOPTION; else return $this->rType_; }
    public function setRType($value) { $this->rType_ = $value; }

    // required .SimplePerson person = 2;

    private $person_ = null;
    public function clearPerson() { $this->person_ = null; }
    public function hasPerson() { return $this->person_ !== null; }
    public function getPerson() { if($this->person_ === null) return null; else return $this->person_; }
    public function setPerson(SimplePerson $value) { $this->person_ = $value; }

    // @@protoc_insertion_point(class_scope:RelationPerson)
  }

  // message EventWitness
  class EventWitness {
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
          throw new Exception('Invalid in parameter');
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
        //var_dump("EventWitness: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->eventWitnessType_ = $tmp;
            $limit-=$len;
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->husband_ = new SimplePerson($fp, $len);
            ASSERT('$len == 0');
            break;
          case 3:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->wife_ = new SimplePerson($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
    }

    function write($fp) {
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
      if (!is_null($this->eventWitnessType_)) {
        fwrite($fp, "\x0a");
        \Protobuf::write_varint($fp, strlen($this->eventWitnessType_));
        fwrite($fp, $this->eventWitnessType_);
      }
      if (!is_null($this->husband_)) {
        fwrite($fp, "\x12");
        \Protobuf::write_varint($fp, $this->husband_->size()); // message
        $this->husband_->write($fp);
      }
      if (!is_null($this->wife_)) {
        fwrite($fp, "\x1a");
        \Protobuf::write_varint($fp, $this->wife_->size()); // message
        $this->wife_->write($fp);
      }
    }

    public function size() {
      $size = 0;
      if (!is_null($this->eventWitnessType_)) {
        $l = strlen($this->eventWitnessType_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->husband_)) {
        $l = $this->husband_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->wife_)) {
        $l = $this->wife_->size();
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }

    public function validateRequired() {
      if ($this->eventWitnessType_ === null) return false;
      if ($this->husband_ === null) return false;
      return true;
    }

    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('eventWitnessType_', $this->eventWitnessType_)
           . \Protobuf::toString('husband_', $this->husband_)
           . \Protobuf::toString('wife_', $this->wife_);
    }

    // required string event_witness_type = 1;

    private $eventWitnessType_ = null;
    public function clearEventWitnessType() { $this->eventWitnessType_ = null; }
    public function hasEventWitnessType() { return $this->eventWitnessType_ !== null; }
    public function getEventWitnessType() { if($this->eventWitnessType_ === null) return ""; else return $this->eventWitnessType_; }
    public function setEventWitnessType($value) { $this->eventWitnessType_ = $value; }

    // required .SimplePerson husband = 2;

    private $husband_ = null;
    public function clearHusband() { $this->husband_ = null; }
    public function hasHusband() { return $this->husband_ !== null; }
    public function getHusband() { if($this->husband_ === null) return null; else return $this->husband_; }
    public function setHusband(SimplePerson $value) { $this->husband_ = $value; }

    // optional .SimplePerson wife = 3;

    private $wife_ = null;
    public function clearWife() { $this->wife_ = null; }
    public function hasWife() { return $this->wife_ !== null; }
    public function getWife() { if($this->wife_ === null) return null; else return $this->wife_; }
    public function setWife(SimplePerson $value) { $this->wife_ = $value; }

    // @@protoc_insertion_point(class_scope:EventWitness)
  }

  // message Person
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
          throw new Exception('Invalid in parameter');
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
        //var_dump("Person: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->index_ = $tmp;

            break;
          case 2:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->sex_ = $tmp;

            break;
          case 3:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->lastname_ = $tmp;
            $limit-=$len;
            break;
          case 4:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->firstname_ = $tmp;
            $limit-=$len;
            break;
          case 5:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->n_ = $tmp;
            $limit-=$len;
            break;
          case 6:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->p_ = $tmp;
            $limit-=$len;
            break;
          case 7:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->occ_ = $tmp;

            break;
          case 8:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->publicName_ = $tmp;
            $limit-=$len;
            break;
          case 9:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->aliases_[] = $tmp;
            $limit-=$len;
            break;
          case 10:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->qualifiers_[] = $tmp;
            $limit-=$len;
            break;
          case 11:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->firstnameAliases_[] = $tmp;
            $limit-=$len;
            break;
          case 12:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->surnameAliases_[] = $tmp;
            $limit-=$len;
            break;
          case 13:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->image_ = $tmp;
            $limit-=$len;
            break;
          case 14:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->birthDate_ = $tmp;
            $limit-=$len;
            break;
          case 15:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->birthDateConv_ = $tmp;
            $limit-=$len;
            break;
          case 16:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->birthDateCal_ = $tmp;

            break;
          case 17:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->birthPlace_ = $tmp;
            $limit-=$len;
            break;
          case 18:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->birthSrc_ = $tmp;
            $limit-=$len;
            break;
          case 19:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->baptismDate_ = $tmp;
            $limit-=$len;
            break;
          case 20:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->baptismDateConv_ = $tmp;
            $limit-=$len;
            break;
          case 21:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->baptismDateCal_ = $tmp;

            break;
          case 22:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->baptismPlace_ = $tmp;
            $limit-=$len;
            break;
          case 23:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->baptismSrc_ = $tmp;
            $limit-=$len;
            break;
          case 24:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->deathDate_ = $tmp;
            $limit-=$len;
            break;
          case 25:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->deathDateConv_ = $tmp;
            $limit-=$len;
            break;
          case 26:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->deathDateCal_ = $tmp;

            break;
          case 27:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->deathPlace_ = $tmp;
            $limit-=$len;
            break;
          case 28:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->deathSrc_ = $tmp;
            $limit-=$len;
            break;
          case 29:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->deathType_ = $tmp;

            break;
          case 30:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->burialDate_ = $tmp;
            $limit-=$len;
            break;
          case 31:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->burialDateConv_ = $tmp;
            $limit-=$len;
            break;
          case 32:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->burialDateCal_ = $tmp;

            break;
          case 33:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->burialPlace_ = $tmp;
            $limit-=$len;
            break;
          case 34:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->burialSrc_ = $tmp;
            $limit-=$len;
            break;
          case 35:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->occupation_ = $tmp;
            $limit-=$len;
            break;
          case 36:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->notes_ = $tmp;
            $limit-=$len;
            break;
          case 37:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->psources_ = $tmp;
            $limit-=$len;
            break;
          case 38:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->hasSources_ = $tmp > 0 ? true : false;
            break;
          case 39:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->titles_[] = $tmp;
            $limit-=$len;
            break;
          case 40:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->related_[] = new RelationPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          case 41:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->rparents_[] = new RelationPerson($fp, $len);
            ASSERT('$len == 0');
            break;
          case 42:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->father_ = new SimplePerson($fp, $len);
            ASSERT('$len == 0');
            break;
          case 43:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->mother_ = new SimplePerson($fp, $len);
            ASSERT('$len == 0');
            break;
          case 44:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->families_[] = new Family($fp, $len);
            ASSERT('$len == 0');
            break;
          case 45:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->sosa_ = $tmp;

            break;
          case 46:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->events_[] = new Event($fp, $len);
            ASSERT('$len == 0');
            break;
          case 47:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->eventsWitnesses_[] = new EventWitness($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
    }

    function write($fp) {
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
      if (!is_null($this->index_)) {
        fwrite($fp, "\x08");
        \Protobuf::write_varint($fp, $this->index_);
      }
      if (!is_null($this->sex_)) {
        fwrite($fp, "\x10");
        \Protobuf::write_varint($fp, $this->sex_);
      }
      if (!is_null($this->lastname_)) {
        fwrite($fp, "\x1a");
        \Protobuf::write_varint($fp, strlen($this->lastname_));
        fwrite($fp, $this->lastname_);
      }
      if (!is_null($this->firstname_)) {
        fwrite($fp, "\"");
        \Protobuf::write_varint($fp, strlen($this->firstname_));
        fwrite($fp, $this->firstname_);
      }
      if (!is_null($this->n_)) {
        fwrite($fp, "*");
        \Protobuf::write_varint($fp, strlen($this->n_));
        fwrite($fp, $this->n_);
      }
      if (!is_null($this->p_)) {
        fwrite($fp, "2");
        \Protobuf::write_varint($fp, strlen($this->p_));
        fwrite($fp, $this->p_);
      }
      if (!is_null($this->occ_)) {
        fwrite($fp, "8");
        \Protobuf::write_varint($fp, $this->occ_);
      }
      if (!is_null($this->publicName_)) {
        fwrite($fp, "B");
        \Protobuf::write_varint($fp, strlen($this->publicName_));
        fwrite($fp, $this->publicName_);
      }
      if (!is_null($this->aliases_))
        foreach($this->aliases_ as $v) {
          fwrite($fp, "J");
          \Protobuf::write_varint($fp, strlen($v));
          fwrite($fp, $v);
        }
      if (!is_null($this->qualifiers_))
        foreach($this->qualifiers_ as $v) {
          fwrite($fp, "R");
          \Protobuf::write_varint($fp, strlen($v));
          fwrite($fp, $v);
        }
      if (!is_null($this->firstnameAliases_))
        foreach($this->firstnameAliases_ as $v) {
          fwrite($fp, "Z");
          \Protobuf::write_varint($fp, strlen($v));
          fwrite($fp, $v);
        }
      if (!is_null($this->surnameAliases_))
        foreach($this->surnameAliases_ as $v) {
          fwrite($fp, "b");
          \Protobuf::write_varint($fp, strlen($v));
          fwrite($fp, $v);
        }
      if (!is_null($this->image_)) {
        fwrite($fp, "j");
        \Protobuf::write_varint($fp, strlen($this->image_));
        fwrite($fp, $this->image_);
      }
      if (!is_null($this->birthDate_)) {
        fwrite($fp, "r");
        \Protobuf::write_varint($fp, strlen($this->birthDate_));
        fwrite($fp, $this->birthDate_);
      }
      if (!is_null($this->birthDateConv_)) {
        fwrite($fp, "z");
        \Protobuf::write_varint($fp, strlen($this->birthDateConv_));
        fwrite($fp, $this->birthDateConv_);
      }
      if (!is_null($this->birthDateCal_)) {
        fwrite($fp, "\x80\x01");
        \Protobuf::write_varint($fp, $this->birthDateCal_);
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
      if (!is_null($this->baptismDateConv_)) {
        fwrite($fp, "\xa2\x01");
        \Protobuf::write_varint($fp, strlen($this->baptismDateConv_));
        fwrite($fp, $this->baptismDateConv_);
      }
      if (!is_null($this->baptismDateCal_)) {
        fwrite($fp, "\xa8\x01");
        \Protobuf::write_varint($fp, $this->baptismDateCal_);
      }
      if (!is_null($this->baptismPlace_)) {
        fwrite($fp, "\xb2\x01");
        \Protobuf::write_varint($fp, strlen($this->baptismPlace_));
        fwrite($fp, $this->baptismPlace_);
      }
      if (!is_null($this->baptismSrc_)) {
        fwrite($fp, "\xba\x01");
        \Protobuf::write_varint($fp, strlen($this->baptismSrc_));
        fwrite($fp, $this->baptismSrc_);
      }
      if (!is_null($this->deathDate_)) {
        fwrite($fp, "\xc2\x01");
        \Protobuf::write_varint($fp, strlen($this->deathDate_));
        fwrite($fp, $this->deathDate_);
      }
      if (!is_null($this->deathDateConv_)) {
        fwrite($fp, "\xca\x01");
        \Protobuf::write_varint($fp, strlen($this->deathDateConv_));
        fwrite($fp, $this->deathDateConv_);
      }
      if (!is_null($this->deathDateCal_)) {
        fwrite($fp, "\xd0\x01");
        \Protobuf::write_varint($fp, $this->deathDateCal_);
      }
      if (!is_null($this->deathPlace_)) {
        fwrite($fp, "\xda\x01");
        \Protobuf::write_varint($fp, strlen($this->deathPlace_));
        fwrite($fp, $this->deathPlace_);
      }
      if (!is_null($this->deathSrc_)) {
        fwrite($fp, "\xe2\x01");
        \Protobuf::write_varint($fp, strlen($this->deathSrc_));
        fwrite($fp, $this->deathSrc_);
      }
      if (!is_null($this->deathType_)) {
        fwrite($fp, "\xe8\x01");
        \Protobuf::write_varint($fp, $this->deathType_);
      }
      if (!is_null($this->burialDate_)) {
        fwrite($fp, "\xf2\x01");
        \Protobuf::write_varint($fp, strlen($this->burialDate_));
        fwrite($fp, $this->burialDate_);
      }
      if (!is_null($this->burialDateConv_)) {
        fwrite($fp, "\xfa\x01");
        \Protobuf::write_varint($fp, strlen($this->burialDateConv_));
        fwrite($fp, $this->burialDateConv_);
      }
      if (!is_null($this->burialDateCal_)) {
        fwrite($fp, "\x80\x02");
        \Protobuf::write_varint($fp, $this->burialDateCal_);
      }
      if (!is_null($this->burialPlace_)) {
        fwrite($fp, "\x8a\x02");
        \Protobuf::write_varint($fp, strlen($this->burialPlace_));
        fwrite($fp, $this->burialPlace_);
      }
      if (!is_null($this->burialSrc_)) {
        fwrite($fp, "\x92\x02");
        \Protobuf::write_varint($fp, strlen($this->burialSrc_));
        fwrite($fp, $this->burialSrc_);
      }
      if (!is_null($this->occupation_)) {
        fwrite($fp, "\x9a\x02");
        \Protobuf::write_varint($fp, strlen($this->occupation_));
        fwrite($fp, $this->occupation_);
      }
      if (!is_null($this->notes_)) {
        fwrite($fp, "\xa2\x02");
        \Protobuf::write_varint($fp, strlen($this->notes_));
        fwrite($fp, $this->notes_);
      }
      if (!is_null($this->psources_)) {
        fwrite($fp, "\xaa\x02");
        \Protobuf::write_varint($fp, strlen($this->psources_));
        fwrite($fp, $this->psources_);
      }
      if (!is_null($this->hasSources_)) {
        fwrite($fp, "\xb0\x02");
        \Protobuf::write_varint($fp, $this->hasSources_ ? 1 : 0);
      }
      if (!is_null($this->titles_))
        foreach($this->titles_ as $v) {
          fwrite($fp, "\xba\x02");
          \Protobuf::write_varint($fp, strlen($v));
          fwrite($fp, $v);
        }
      if (!is_null($this->related_))
        foreach($this->related_ as $v) {
          fwrite($fp, "\xc2\x02");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->rparents_))
        foreach($this->rparents_ as $v) {
          fwrite($fp, "\xca\x02");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->father_)) {
        fwrite($fp, "\xd2\x02");
        \Protobuf::write_varint($fp, $this->father_->size()); // message
        $this->father_->write($fp);
      }
      if (!is_null($this->mother_)) {
        fwrite($fp, "\xda\x02");
        \Protobuf::write_varint($fp, $this->mother_->size()); // message
        $this->mother_->write($fp);
      }
      if (!is_null($this->families_))
        foreach($this->families_ as $v) {
          fwrite($fp, "\xe2\x02");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->sosa_)) {
        fwrite($fp, "\xe8\x02");
        \Protobuf::write_varint($fp, $this->sosa_);
      }
      if (!is_null($this->events_))
        foreach($this->events_ as $v) {
          fwrite($fp, "\xf2\x02");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->eventsWitnesses_))
        foreach($this->eventsWitnesses_ as $v) {
          fwrite($fp, "\xfa\x02");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
    }

    public function size() {
      $size = 0;
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
      if (!is_null($this->n_)) {
        $l = strlen($this->n_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->p_)) {
        $l = strlen($this->p_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->occ_)) {
        $size += 1 + \Protobuf::size_varint($this->occ_);
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
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->birthDateConv_)) {
        $l = strlen($this->birthDateConv_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->birthDateCal_)) {
        $size += 2 + \Protobuf::size_varint($this->birthDateCal_);
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
      if (!is_null($this->baptismDateConv_)) {
        $l = strlen($this->baptismDateConv_);
        $size += 2 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->baptismDateCal_)) {
        $size += 2 + \Protobuf::size_varint($this->baptismDateCal_);
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
      if (!is_null($this->deathDateConv_)) {
        $l = strlen($this->deathDateConv_);
        $size += 2 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->deathDateCal_)) {
        $size += 2 + \Protobuf::size_varint($this->deathDateCal_);
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
      if (!is_null($this->burialDateConv_)) {
        $l = strlen($this->burialDateConv_);
        $size += 2 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->burialDateCal_)) {
        $size += 2 + \Protobuf::size_varint($this->burialDateCal_);
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
      if (!is_null($this->notes_)) {
        $l = strlen($this->notes_);
        $size += 2 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->psources_)) {
        $l = strlen($this->psources_);
        $size += 2 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->hasSources_)) {
        $size += 3;
      }
      if (!is_null($this->titles_))
        foreach($this->titles_ as $v) {
          $l = strlen($v);
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
      if (!is_null($this->father_)) {
        $l = $this->father_->size();
        $size += 2 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->mother_)) {
        $l = $this->mother_->size();
        $size += 2 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->families_))
        foreach($this->families_ as $v) {
          $l = $v->size();
          $size += 2 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->sosa_)) {
        $size += 2 + \Protobuf::size_varint($this->sosa_);
      }
      if (!is_null($this->events_))
        foreach($this->events_ as $v) {
          $l = $v->size();
          $size += 2 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->eventsWitnesses_))
        foreach($this->eventsWitnesses_ as $v) {
          $l = $v->size();
          $size += 2 + \Protobuf::size_varint($l) + $l;
        }
      return $size;
    }

    public function validateRequired() {
      if ($this->index_ === null) return false;
      if ($this->sex_ === null) return false;
      if ($this->lastname_ === null) return false;
      if ($this->firstname_ === null) return false;
      if ($this->n_ === null) return false;
      if ($this->p_ === null) return false;
      if ($this->occ_ === null) return false;
      if ($this->deathType_ === null) return false;
      if ($this->hasSources_ === null) return false;
      if ($this->sosa_ === null) return false;
      return true;
    }

    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('index_', $this->index_)
           . \Protobuf::toString('sex_', Sex::toString($this->sex_))
           . \Protobuf::toString('lastname_', $this->lastname_)
           . \Protobuf::toString('firstname_', $this->firstname_)
           . \Protobuf::toString('n_', $this->n_)
           . \Protobuf::toString('p_', $this->p_)
           . \Protobuf::toString('occ_', $this->occ_)
           . \Protobuf::toString('publicName_', $this->publicName_)
           . \Protobuf::toString('aliases_', $this->aliases_)
           . \Protobuf::toString('qualifiers_', $this->qualifiers_)
           . \Protobuf::toString('firstnameAliases_', $this->firstnameAliases_)
           . \Protobuf::toString('surnameAliases_', $this->surnameAliases_)
           . \Protobuf::toString('image_', $this->image_)
           . \Protobuf::toString('birthDate_', $this->birthDate_)
           . \Protobuf::toString('birthDateConv_', $this->birthDateConv_)
           . \Protobuf::toString('birthDateCal_', Calendar::toString($this->birthDateCal_))
           . \Protobuf::toString('birthPlace_', $this->birthPlace_)
           . \Protobuf::toString('birthSrc_', $this->birthSrc_)
           . \Protobuf::toString('baptismDate_', $this->baptismDate_)
           . \Protobuf::toString('baptismDateConv_', $this->baptismDateConv_)
           . \Protobuf::toString('baptismDateCal_', Calendar::toString($this->baptismDateCal_))
           . \Protobuf::toString('baptismPlace_', $this->baptismPlace_)
           . \Protobuf::toString('baptismSrc_', $this->baptismSrc_)
           . \Protobuf::toString('deathDate_', $this->deathDate_)
           . \Protobuf::toString('deathDateConv_', $this->deathDateConv_)
           . \Protobuf::toString('deathDateCal_', Calendar::toString($this->deathDateCal_))
           . \Protobuf::toString('deathPlace_', $this->deathPlace_)
           . \Protobuf::toString('deathSrc_', $this->deathSrc_)
           . \Protobuf::toString('deathType_', DeathType::toString($this->deathType_))
           . \Protobuf::toString('burialDate_', $this->burialDate_)
           . \Protobuf::toString('burialDateConv_', $this->burialDateConv_)
           . \Protobuf::toString('burialDateCal_', Calendar::toString($this->burialDateCal_))
           . \Protobuf::toString('burialPlace_', $this->burialPlace_)
           . \Protobuf::toString('burialSrc_', $this->burialSrc_)
           . \Protobuf::toString('occupation_', $this->occupation_)
           . \Protobuf::toString('notes_', $this->notes_)
           . \Protobuf::toString('psources_', $this->psources_)
           . \Protobuf::toString('hasSources_', $this->hasSources_)
           . \Protobuf::toString('titles_', $this->titles_)
           . \Protobuf::toString('related_', $this->related_)
           . \Protobuf::toString('rparents_', $this->rparents_)
           . \Protobuf::toString('father_', $this->father_)
           . \Protobuf::toString('mother_', $this->mother_)
           . \Protobuf::toString('families_', $this->families_)
           . \Protobuf::toString('sosa_', Sosa::toString($this->sosa_))
           . \Protobuf::toString('events_', $this->events_)
           . \Protobuf::toString('eventsWitnesses_', $this->eventsWitnesses_);
    }

    // required int32 index = 1;

    private $index_ = null;
    public function clearIndex() { $this->index_ = null; }
    public function hasIndex() { return $this->index_ !== null; }
    public function getIndex() { if($this->index_ === null) return 0; else return $this->index_; }
    public function setIndex($value) { $this->index_ = $value; }

    // required .Sex sex = 2;

    private $sex_ = null;
    public function clearSex() { $this->sex_ = null; }
    public function hasSex() { return $this->sex_ !== null; }
    public function getSex() { if($this->sex_ === null) return Sex::MALE; else return $this->sex_; }
    public function setSex($value) { $this->sex_ = $value; }

    // required string lastname = 3;

    private $lastname_ = null;
    public function clearLastname() { $this->lastname_ = null; }
    public function hasLastname() { return $this->lastname_ !== null; }
    public function getLastname() { if($this->lastname_ === null) return ""; else return $this->lastname_; }
    public function setLastname($value) { $this->lastname_ = $value; }

    // required string firstname = 4;

    private $firstname_ = null;
    public function clearFirstname() { $this->firstname_ = null; }
    public function hasFirstname() { return $this->firstname_ !== null; }
    public function getFirstname() { if($this->firstname_ === null) return ""; else return $this->firstname_; }
    public function setFirstname($value) { $this->firstname_ = $value; }

    // required string n = 5;

    private $n_ = null;
    public function clearN() { $this->n_ = null; }
    public function hasN() { return $this->n_ !== null; }
    public function getN() { if($this->n_ === null) return ""; else return $this->n_; }
    public function setN($value) { $this->n_ = $value; }

    // required string p = 6;

    private $p_ = null;
    public function clearP() { $this->p_ = null; }
    public function hasP() { return $this->p_ !== null; }
    public function getP() { if($this->p_ === null) return ""; else return $this->p_; }
    public function setP($value) { $this->p_ = $value; }

    // required int32 occ = 7;

    private $occ_ = null;
    public function clearOcc() { $this->occ_ = null; }
    public function hasOcc() { return $this->occ_ !== null; }
    public function getOcc() { if($this->occ_ === null) return 0; else return $this->occ_; }
    public function setOcc($value) { $this->occ_ = $value; }

    // optional string public_name = 8;

    private $publicName_ = null;
    public function clearPublicName() { $this->publicName_ = null; }
    public function hasPublicName() { return $this->publicName_ !== null; }
    public function getPublicName() { if($this->publicName_ === null) return ""; else return $this->publicName_; }
    public function setPublicName($value) { $this->publicName_ = $value; }

    // repeated string aliases = 9;

    private $aliases_ = null;
    public function clearAliases() { $this->aliases_ = null; }
    public function getAliasesCount() { if ($this->aliases_ === null ) return 0; else return count($this->aliases_); }
    public function getAliases($index) { return $this->aliases_[$index]; }
    public function getAliasesArray() { if ($this->aliases_ === null ) return array(); else return $this->aliases_; }
    public function setAliases($index, $value) {$this->aliases_[$index] = $value;	}
    public function addAliases($value) { $this->aliases_[] = $value; }
    public function addAllAliases(array $values) { foreach($values as $value) {$this->aliases_[] = $value;} }

    // repeated string qualifiers = 10;

    private $qualifiers_ = null;
    public function clearQualifiers() { $this->qualifiers_ = null; }
    public function getQualifiersCount() { if ($this->qualifiers_ === null ) return 0; else return count($this->qualifiers_); }
    public function getQualifiers($index) { return $this->qualifiers_[$index]; }
    public function getQualifiersArray() { if ($this->qualifiers_ === null ) return array(); else return $this->qualifiers_; }
    public function setQualifiers($index, $value) {$this->qualifiers_[$index] = $value;	}
    public function addQualifiers($value) { $this->qualifiers_[] = $value; }
    public function addAllQualifiers(array $values) { foreach($values as $value) {$this->qualifiers_[] = $value;} }

    // repeated string firstname_aliases = 11;

    private $firstnameAliases_ = null;
    public function clearFirstnameAliases() { $this->firstnameAliases_ = null; }
    public function getFirstnameAliasesCount() { if ($this->firstnameAliases_ === null ) return 0; else return count($this->firstnameAliases_); }
    public function getFirstnameAliases($index) { return $this->firstnameAliases_[$index]; }
    public function getFirstnameAliasesArray() { if ($this->firstnameAliases_ === null ) return array(); else return $this->firstnameAliases_; }
    public function setFirstnameAliases($index, $value) {$this->firstnameAliases_[$index] = $value;	}
    public function addFirstnameAliases($value) { $this->firstnameAliases_[] = $value; }
    public function addAllFirstnameAliases(array $values) { foreach($values as $value) {$this->firstnameAliases_[] = $value;} }

    // repeated string surname_aliases = 12;

    private $surnameAliases_ = null;
    public function clearSurnameAliases() { $this->surnameAliases_ = null; }
    public function getSurnameAliasesCount() { if ($this->surnameAliases_ === null ) return 0; else return count($this->surnameAliases_); }
    public function getSurnameAliases($index) { return $this->surnameAliases_[$index]; }
    public function getSurnameAliasesArray() { if ($this->surnameAliases_ === null ) return array(); else return $this->surnameAliases_; }
    public function setSurnameAliases($index, $value) {$this->surnameAliases_[$index] = $value;	}
    public function addSurnameAliases($value) { $this->surnameAliases_[] = $value; }
    public function addAllSurnameAliases(array $values) { foreach($values as $value) {$this->surnameAliases_[] = $value;} }

    // optional string image = 13;

    private $image_ = null;
    public function clearImage() { $this->image_ = null; }
    public function hasImage() { return $this->image_ !== null; }
    public function getImage() { if($this->image_ === null) return ""; else return $this->image_; }
    public function setImage($value) { $this->image_ = $value; }

    // optional string birth_date = 14;

    private $birthDate_ = null;
    public function clearBirthDate() { $this->birthDate_ = null; }
    public function hasBirthDate() { return $this->birthDate_ !== null; }
    public function getBirthDate() { if($this->birthDate_ === null) return ""; else return $this->birthDate_; }
    public function setBirthDate($value) { $this->birthDate_ = $value; }

    // optional string birth_date_conv = 15;

    private $birthDateConv_ = null;
    public function clearBirthDateConv() { $this->birthDateConv_ = null; }
    public function hasBirthDateConv() { return $this->birthDateConv_ !== null; }
    public function getBirthDateConv() { if($this->birthDateConv_ === null) return ""; else return $this->birthDateConv_; }
    public function setBirthDateConv($value) { $this->birthDateConv_ = $value; }

    // optional .Calendar birth_date_cal = 16;

    private $birthDateCal_ = null;
    public function clearBirthDateCal() { $this->birthDateCal_ = null; }
    public function hasBirthDateCal() { return $this->birthDateCal_ !== null; }
    public function getBirthDateCal() { if($this->birthDateCal_ === null) return Calendar::GREGORIAN; else return $this->birthDateCal_; }
    public function setBirthDateCal($value) { $this->birthDateCal_ = $value; }

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

    // optional string baptism_date_conv = 20;

    private $baptismDateConv_ = null;
    public function clearBaptismDateConv() { $this->baptismDateConv_ = null; }
    public function hasBaptismDateConv() { return $this->baptismDateConv_ !== null; }
    public function getBaptismDateConv() { if($this->baptismDateConv_ === null) return ""; else return $this->baptismDateConv_; }
    public function setBaptismDateConv($value) { $this->baptismDateConv_ = $value; }

    // optional .Calendar baptism_date_cal = 21;

    private $baptismDateCal_ = null;
    public function clearBaptismDateCal() { $this->baptismDateCal_ = null; }
    public function hasBaptismDateCal() { return $this->baptismDateCal_ !== null; }
    public function getBaptismDateCal() { if($this->baptismDateCal_ === null) return Calendar::GREGORIAN; else return $this->baptismDateCal_; }
    public function setBaptismDateCal($value) { $this->baptismDateCal_ = $value; }

    // optional string baptism_place = 22;

    private $baptismPlace_ = null;
    public function clearBaptismPlace() { $this->baptismPlace_ = null; }
    public function hasBaptismPlace() { return $this->baptismPlace_ !== null; }
    public function getBaptismPlace() { if($this->baptismPlace_ === null) return ""; else return $this->baptismPlace_; }
    public function setBaptismPlace($value) { $this->baptismPlace_ = $value; }

    // optional string baptism_src = 23;

    private $baptismSrc_ = null;
    public function clearBaptismSrc() { $this->baptismSrc_ = null; }
    public function hasBaptismSrc() { return $this->baptismSrc_ !== null; }
    public function getBaptismSrc() { if($this->baptismSrc_ === null) return ""; else return $this->baptismSrc_; }
    public function setBaptismSrc($value) { $this->baptismSrc_ = $value; }

    // optional string death_date = 24;

    private $deathDate_ = null;
    public function clearDeathDate() { $this->deathDate_ = null; }
    public function hasDeathDate() { return $this->deathDate_ !== null; }
    public function getDeathDate() { if($this->deathDate_ === null) return ""; else return $this->deathDate_; }
    public function setDeathDate($value) { $this->deathDate_ = $value; }

    // optional string death_date_conv = 25;

    private $deathDateConv_ = null;
    public function clearDeathDateConv() { $this->deathDateConv_ = null; }
    public function hasDeathDateConv() { return $this->deathDateConv_ !== null; }
    public function getDeathDateConv() { if($this->deathDateConv_ === null) return ""; else return $this->deathDateConv_; }
    public function setDeathDateConv($value) { $this->deathDateConv_ = $value; }

    // optional .Calendar death_date_cal = 26;

    private $deathDateCal_ = null;
    public function clearDeathDateCal() { $this->deathDateCal_ = null; }
    public function hasDeathDateCal() { return $this->deathDateCal_ !== null; }
    public function getDeathDateCal() { if($this->deathDateCal_ === null) return Calendar::GREGORIAN; else return $this->deathDateCal_; }
    public function setDeathDateCal($value) { $this->deathDateCal_ = $value; }

    // optional string death_place = 27;

    private $deathPlace_ = null;
    public function clearDeathPlace() { $this->deathPlace_ = null; }
    public function hasDeathPlace() { return $this->deathPlace_ !== null; }
    public function getDeathPlace() { if($this->deathPlace_ === null) return ""; else return $this->deathPlace_; }
    public function setDeathPlace($value) { $this->deathPlace_ = $value; }

    // optional string death_src = 28;

    private $deathSrc_ = null;
    public function clearDeathSrc() { $this->deathSrc_ = null; }
    public function hasDeathSrc() { return $this->deathSrc_ !== null; }
    public function getDeathSrc() { if($this->deathSrc_ === null) return ""; else return $this->deathSrc_; }
    public function setDeathSrc($value) { $this->deathSrc_ = $value; }

    // required .DeathType death_type = 29;

    private $deathType_ = null;
    public function clearDeathType() { $this->deathType_ = null; }
    public function hasDeathType() { return $this->deathType_ !== null; }
    public function getDeathType() { if($this->deathType_ === null) return DeathType::NOT_DEAD; else return $this->deathType_; }
    public function setDeathType($value) { $this->deathType_ = $value; }

    // optional string burial_date = 30;

    private $burialDate_ = null;
    public function clearBurialDate() { $this->burialDate_ = null; }
    public function hasBurialDate() { return $this->burialDate_ !== null; }
    public function getBurialDate() { if($this->burialDate_ === null) return ""; else return $this->burialDate_; }
    public function setBurialDate($value) { $this->burialDate_ = $value; }

    // optional string burial_date_conv = 31;

    private $burialDateConv_ = null;
    public function clearBurialDateConv() { $this->burialDateConv_ = null; }
    public function hasBurialDateConv() { return $this->burialDateConv_ !== null; }
    public function getBurialDateConv() { if($this->burialDateConv_ === null) return ""; else return $this->burialDateConv_; }
    public function setBurialDateConv($value) { $this->burialDateConv_ = $value; }

    // optional .Calendar burial_date_cal = 32;

    private $burialDateCal_ = null;
    public function clearBurialDateCal() { $this->burialDateCal_ = null; }
    public function hasBurialDateCal() { return $this->burialDateCal_ !== null; }
    public function getBurialDateCal() { if($this->burialDateCal_ === null) return Calendar::GREGORIAN; else return $this->burialDateCal_; }
    public function setBurialDateCal($value) { $this->burialDateCal_ = $value; }

    // optional string burial_place = 33;

    private $burialPlace_ = null;
    public function clearBurialPlace() { $this->burialPlace_ = null; }
    public function hasBurialPlace() { return $this->burialPlace_ !== null; }
    public function getBurialPlace() { if($this->burialPlace_ === null) return ""; else return $this->burialPlace_; }
    public function setBurialPlace($value) { $this->burialPlace_ = $value; }

    // optional string burial_src = 34;

    private $burialSrc_ = null;
    public function clearBurialSrc() { $this->burialSrc_ = null; }
    public function hasBurialSrc() { return $this->burialSrc_ !== null; }
    public function getBurialSrc() { if($this->burialSrc_ === null) return ""; else return $this->burialSrc_; }
    public function setBurialSrc($value) { $this->burialSrc_ = $value; }

    // optional string occupation = 35;

    private $occupation_ = null;
    public function clearOccupation() { $this->occupation_ = null; }
    public function hasOccupation() { return $this->occupation_ !== null; }
    public function getOccupation() { if($this->occupation_ === null) return ""; else return $this->occupation_; }
    public function setOccupation($value) { $this->occupation_ = $value; }

    // optional string notes = 36;

    private $notes_ = null;
    public function clearNotes() { $this->notes_ = null; }
    public function hasNotes() { return $this->notes_ !== null; }
    public function getNotes() { if($this->notes_ === null) return ""; else return $this->notes_; }
    public function setNotes($value) { $this->notes_ = $value; }

    // optional string psources = 37;

    private $psources_ = null;
    public function clearPsources() { $this->psources_ = null; }
    public function hasPsources() { return $this->psources_ !== null; }
    public function getPsources() { if($this->psources_ === null) return ""; else return $this->psources_; }
    public function setPsources($value) { $this->psources_ = $value; }

    // required bool has_sources = 38;

    private $hasSources_ = null;
    public function clearHasSources() { $this->hasSources_ = null; }
    public function hasHasSources() { return $this->hasSources_ !== null; }
    public function getHasSources() { if($this->hasSources_ === null) return false; else return $this->hasSources_; }
    public function setHasSources($value) { $this->hasSources_ = $value; }

    // repeated string titles = 39;

    private $titles_ = null;
    public function clearTitles() { $this->titles_ = null; }
    public function getTitlesCount() { if ($this->titles_ === null ) return 0; else return count($this->titles_); }
    public function getTitles($index) { return $this->titles_[$index]; }
    public function getTitlesArray() { if ($this->titles_ === null ) return array(); else return $this->titles_; }
    public function setTitles($index, $value) {$this->titles_[$index] = $value;	}
    public function addTitles($value) { $this->titles_[] = $value; }
    public function addAllTitles(array $values) { foreach($values as $value) {$this->titles_[] = $value;} }

    // repeated .RelationPerson related = 40;

    private $related_ = null;
    public function clearRelated() { $this->related_ = null; }
    public function getRelatedCount() { if ($this->related_ === null ) return 0; else return count($this->related_); }
    public function getRelated($index) { return $this->related_[$index]; }
    public function getRelatedArray() { if ($this->related_ === null ) return array(); else return $this->related_; }
    public function setRelated($index, $value) {$this->related_[$index] = $value;	}
    public function addRelated($value) { $this->related_[] = $value; }
    public function addAllRelated(array $values) { foreach($values as $value) {$this->related_[] = $value;} }

    // repeated .RelationPerson rparents = 41;

    private $rparents_ = null;
    public function clearRparents() { $this->rparents_ = null; }
    public function getRparentsCount() { if ($this->rparents_ === null ) return 0; else return count($this->rparents_); }
    public function getRparents($index) { return $this->rparents_[$index]; }
    public function getRparentsArray() { if ($this->rparents_ === null ) return array(); else return $this->rparents_; }
    public function setRparents($index, $value) {$this->rparents_[$index] = $value;	}
    public function addRparents($value) { $this->rparents_[] = $value; }
    public function addAllRparents(array $values) { foreach($values as $value) {$this->rparents_[] = $value;} }

    // optional .SimplePerson father = 42;

    private $father_ = null;
    public function clearFather() { $this->father_ = null; }
    public function hasFather() { return $this->father_ !== null; }
    public function getFather() { if($this->father_ === null) return null; else return $this->father_; }
    public function setFather(SimplePerson $value) { $this->father_ = $value; }

    // optional .SimplePerson mother = 43;

    private $mother_ = null;
    public function clearMother() { $this->mother_ = null; }
    public function hasMother() { return $this->mother_ !== null; }
    public function getMother() { if($this->mother_ === null) return null; else return $this->mother_; }
    public function setMother(SimplePerson $value) { $this->mother_ = $value; }

    // repeated .Family families = 44;

    private $families_ = null;
    public function clearFamilies() { $this->families_ = null; }
    public function getFamiliesCount() { if ($this->families_ === null ) return 0; else return count($this->families_); }
    public function getFamilies($index) { return $this->families_[$index]; }
    public function getFamiliesArray() { if ($this->families_ === null ) return array(); else return $this->families_; }
    public function setFamilies($index, $value) {$this->families_[$index] = $value;	}
    public function addFamilies($value) { $this->families_[] = $value; }
    public function addAllFamilies(array $values) { foreach($values as $value) {$this->families_[] = $value;} }

    // required .Sosa sosa = 45;

    private $sosa_ = null;
    public function clearSosa() { $this->sosa_ = null; }
    public function hasSosa() { return $this->sosa_ !== null; }
    public function getSosa() { if($this->sosa_ === null) return Sosa::SOSA_REF; else return $this->sosa_; }
    public function setSosa($value) { $this->sosa_ = $value; }

    // repeated .Event events = 46;

    private $events_ = null;
    public function clearEvents() { $this->events_ = null; }
    public function getEventsCount() { if ($this->events_ === null ) return 0; else return count($this->events_); }
    public function getEvents($index) { return $this->events_[$index]; }
    public function getEventsArray() { if ($this->events_ === null ) return array(); else return $this->events_; }
    public function setEvents($index, $value) {$this->events_[$index] = $value;	}
    public function addEvents($value) { $this->events_[] = $value; }
    public function addAllEvents(array $values) { foreach($values as $value) {$this->events_[] = $value;} }

    // repeated .EventWitness events_witnesses = 47;

    private $eventsWitnesses_ = null;
    public function clearEventsWitnesses() { $this->eventsWitnesses_ = null; }
    public function getEventsWitnessesCount() { if ($this->eventsWitnesses_ === null ) return 0; else return count($this->eventsWitnesses_); }
    public function getEventsWitnesses($index) { return $this->eventsWitnesses_[$index]; }
    public function getEventsWitnessesArray() { if ($this->eventsWitnesses_ === null ) return array(); else return $this->eventsWitnesses_; }
    public function setEventsWitnesses($index, $value) {$this->eventsWitnesses_[$index] = $value;	}
    public function addEventsWitnesses($value) { $this->eventsWitnesses_[] = $value; }
    public function addAllEventsWitnesses(array $values) { foreach($values as $value) {$this->eventsWitnesses_[] = $value;} }

    // @@protoc_insertion_point(class_scope:Person)
  }

  // message Family
  class Family {
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
          throw new Exception('Invalid in parameter');
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
        //var_dump("Family: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->index_ = $tmp;

            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->spouse_ = new SimplePerson($fp, $len);
            ASSERT('$len == 0');
            break;
          case 3:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->marriageDate_ = $tmp;
            $limit-=$len;
            break;
          case 4:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->marriageDateConv_ = $tmp;
            $limit-=$len;
            break;
          case 5:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->marriageDateCal_ = $tmp;

            break;
          case 6:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->marriagePlace_ = $tmp;
            $limit-=$len;
            break;
          case 7:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->marriageSrc_ = $tmp;
            $limit-=$len;
            break;
          case 8:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->marriageType_ = $tmp;

            break;
          case 9:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->divorceType_ = $tmp;

            break;
          case 10:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->divorceDate_ = $tmp;
            $limit-=$len;
            break;
          case 11:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->divorceDateConv_ = $tmp;
            $limit-=$len;
            break;
          case 12:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->divorceDateCal_ = $tmp;

            break;
          case 13:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->witnesses_[] = new SimplePerson($fp, $len);
            ASSERT('$len == 0');
            break;
          case 14:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->notes_ = $tmp;
            $limit-=$len;
            break;
          case 15:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->fsources_ = $tmp;
            $limit-=$len;
            break;
          case 16:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->children_[] = new SimplePerson($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
    }

    function write($fp) {
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
      if (!is_null($this->index_)) {
        fwrite($fp, "\x08");
        \Protobuf::write_varint($fp, $this->index_);
      }
      if (!is_null($this->spouse_)) {
        fwrite($fp, "\x12");
        \Protobuf::write_varint($fp, $this->spouse_->size()); // message
        $this->spouse_->write($fp);
      }
      if (!is_null($this->marriageDate_)) {
        fwrite($fp, "\x1a");
        \Protobuf::write_varint($fp, strlen($this->marriageDate_));
        fwrite($fp, $this->marriageDate_);
      }
      if (!is_null($this->marriageDateConv_)) {
        fwrite($fp, "\"");
        \Protobuf::write_varint($fp, strlen($this->marriageDateConv_));
        fwrite($fp, $this->marriageDateConv_);
      }
      if (!is_null($this->marriageDateCal_)) {
        fwrite($fp, "(");
        \Protobuf::write_varint($fp, $this->marriageDateCal_);
      }
      if (!is_null($this->marriagePlace_)) {
        fwrite($fp, "2");
        \Protobuf::write_varint($fp, strlen($this->marriagePlace_));
        fwrite($fp, $this->marriagePlace_);
      }
      if (!is_null($this->marriageSrc_)) {
        fwrite($fp, ":");
        \Protobuf::write_varint($fp, strlen($this->marriageSrc_));
        fwrite($fp, $this->marriageSrc_);
      }
      if (!is_null($this->marriageType_)) {
        fwrite($fp, "@");
        \Protobuf::write_varint($fp, $this->marriageType_);
      }
      if (!is_null($this->divorceType_)) {
        fwrite($fp, "H");
        \Protobuf::write_varint($fp, $this->divorceType_);
      }
      if (!is_null($this->divorceDate_)) {
        fwrite($fp, "R");
        \Protobuf::write_varint($fp, strlen($this->divorceDate_));
        fwrite($fp, $this->divorceDate_);
      }
      if (!is_null($this->divorceDateConv_)) {
        fwrite($fp, "Z");
        \Protobuf::write_varint($fp, strlen($this->divorceDateConv_));
        fwrite($fp, $this->divorceDateConv_);
      }
      if (!is_null($this->divorceDateCal_)) {
        fwrite($fp, "`");
        \Protobuf::write_varint($fp, $this->divorceDateCal_);
      }
      if (!is_null($this->witnesses_))
        foreach($this->witnesses_ as $v) {
          fwrite($fp, "j");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->notes_)) {
        fwrite($fp, "r");
        \Protobuf::write_varint($fp, strlen($this->notes_));
        fwrite($fp, $this->notes_);
      }
      if (!is_null($this->fsources_)) {
        fwrite($fp, "z");
        \Protobuf::write_varint($fp, strlen($this->fsources_));
        fwrite($fp, $this->fsources_);
      }
      if (!is_null($this->children_))
        foreach($this->children_ as $v) {
          fwrite($fp, "\x82\x01");
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
      if (!is_null($this->marriageDate_)) {
        $l = strlen($this->marriageDate_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->marriageDateConv_)) {
        $l = strlen($this->marriageDateConv_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->marriageDateCal_)) {
        $size += 1 + \Protobuf::size_varint($this->marriageDateCal_);
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
      if (!is_null($this->divorceDateConv_)) {
        $l = strlen($this->divorceDateConv_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->divorceDateCal_)) {
        $size += 1 + \Protobuf::size_varint($this->divorceDateCal_);
      }
      if (!is_null($this->witnesses_))
        foreach($this->witnesses_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->notes_)) {
        $l = strlen($this->notes_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->fsources_)) {
        $l = strlen($this->fsources_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->children_))
        foreach($this->children_ as $v) {
          $l = $v->size();
          $size += 2 + \Protobuf::size_varint($l) + $l;
        }
      return $size;
    }

    public function validateRequired() {
      if ($this->index_ === null) return false;
      if ($this->spouse_ === null) return false;
      if ($this->marriageType_ === null) return false;
      if ($this->divorceType_ === null) return false;
      return true;
    }

    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('index_', $this->index_)
           . \Protobuf::toString('spouse_', $this->spouse_)
           . \Protobuf::toString('marriageDate_', $this->marriageDate_)
           . \Protobuf::toString('marriageDateConv_', $this->marriageDateConv_)
           . \Protobuf::toString('marriageDateCal_', Calendar::toString($this->marriageDateCal_))
           . \Protobuf::toString('marriagePlace_', $this->marriagePlace_)
           . \Protobuf::toString('marriageSrc_', $this->marriageSrc_)
           . \Protobuf::toString('marriageType_', MarriageType::toString($this->marriageType_))
           . \Protobuf::toString('divorceType_', DivorceType::toString($this->divorceType_))
           . \Protobuf::toString('divorceDate_', $this->divorceDate_)
           . \Protobuf::toString('divorceDateConv_', $this->divorceDateConv_)
           . \Protobuf::toString('divorceDateCal_', Calendar::toString($this->divorceDateCal_))
           . \Protobuf::toString('witnesses_', $this->witnesses_)
           . \Protobuf::toString('notes_', $this->notes_)
           . \Protobuf::toString('fsources_', $this->fsources_)
           . \Protobuf::toString('children_', $this->children_);
    }

    // required int32 index = 1;

    private $index_ = null;
    public function clearIndex() { $this->index_ = null; }
    public function hasIndex() { return $this->index_ !== null; }
    public function getIndex() { if($this->index_ === null) return 0; else return $this->index_; }
    public function setIndex($value) { $this->index_ = $value; }

    // required .SimplePerson spouse = 2;

    private $spouse_ = null;
    public function clearSpouse() { $this->spouse_ = null; }
    public function hasSpouse() { return $this->spouse_ !== null; }
    public function getSpouse() { if($this->spouse_ === null) return null; else return $this->spouse_; }
    public function setSpouse(SimplePerson $value) { $this->spouse_ = $value; }

    // optional string marriage_date = 3;

    private $marriageDate_ = null;
    public function clearMarriageDate() { $this->marriageDate_ = null; }
    public function hasMarriageDate() { return $this->marriageDate_ !== null; }
    public function getMarriageDate() { if($this->marriageDate_ === null) return ""; else return $this->marriageDate_; }
    public function setMarriageDate($value) { $this->marriageDate_ = $value; }

    // optional string marriage_date_conv = 4;

    private $marriageDateConv_ = null;
    public function clearMarriageDateConv() { $this->marriageDateConv_ = null; }
    public function hasMarriageDateConv() { return $this->marriageDateConv_ !== null; }
    public function getMarriageDateConv() { if($this->marriageDateConv_ === null) return ""; else return $this->marriageDateConv_; }
    public function setMarriageDateConv($value) { $this->marriageDateConv_ = $value; }

    // optional .Calendar marriage_date_cal = 5;

    private $marriageDateCal_ = null;
    public function clearMarriageDateCal() { $this->marriageDateCal_ = null; }
    public function hasMarriageDateCal() { return $this->marriageDateCal_ !== null; }
    public function getMarriageDateCal() { if($this->marriageDateCal_ === null) return Calendar::GREGORIAN; else return $this->marriageDateCal_; }
    public function setMarriageDateCal($value) { $this->marriageDateCal_ = $value; }

    // optional string marriage_place = 6;

    private $marriagePlace_ = null;
    public function clearMarriagePlace() { $this->marriagePlace_ = null; }
    public function hasMarriagePlace() { return $this->marriagePlace_ !== null; }
    public function getMarriagePlace() { if($this->marriagePlace_ === null) return ""; else return $this->marriagePlace_; }
    public function setMarriagePlace($value) { $this->marriagePlace_ = $value; }

    // optional string marriage_src = 7;

    private $marriageSrc_ = null;
    public function clearMarriageSrc() { $this->marriageSrc_ = null; }
    public function hasMarriageSrc() { return $this->marriageSrc_ !== null; }
    public function getMarriageSrc() { if($this->marriageSrc_ === null) return ""; else return $this->marriageSrc_; }
    public function setMarriageSrc($value) { $this->marriageSrc_ = $value; }

    // required .MarriageType marriage_type = 8;

    private $marriageType_ = null;
    public function clearMarriageType() { $this->marriageType_ = null; }
    public function hasMarriageType() { return $this->marriageType_ !== null; }
    public function getMarriageType() { if($this->marriageType_ === null) return MarriageType::MARRIED; else return $this->marriageType_; }
    public function setMarriageType($value) { $this->marriageType_ = $value; }

    // required .DivorceType divorce_type = 9;

    private $divorceType_ = null;
    public function clearDivorceType() { $this->divorceType_ = null; }
    public function hasDivorceType() { return $this->divorceType_ !== null; }
    public function getDivorceType() { if($this->divorceType_ === null) return DivorceType::NOT_DIVORCED; else return $this->divorceType_; }
    public function setDivorceType($value) { $this->divorceType_ = $value; }

    // optional string divorce_date = 10;

    private $divorceDate_ = null;
    public function clearDivorceDate() { $this->divorceDate_ = null; }
    public function hasDivorceDate() { return $this->divorceDate_ !== null; }
    public function getDivorceDate() { if($this->divorceDate_ === null) return ""; else return $this->divorceDate_; }
    public function setDivorceDate($value) { $this->divorceDate_ = $value; }

    // optional string divorce_date_conv = 11;

    private $divorceDateConv_ = null;
    public function clearDivorceDateConv() { $this->divorceDateConv_ = null; }
    public function hasDivorceDateConv() { return $this->divorceDateConv_ !== null; }
    public function getDivorceDateConv() { if($this->divorceDateConv_ === null) return ""; else return $this->divorceDateConv_; }
    public function setDivorceDateConv($value) { $this->divorceDateConv_ = $value; }

    // optional .Calendar divorce_date_cal = 12;

    private $divorceDateCal_ = null;
    public function clearDivorceDateCal() { $this->divorceDateCal_ = null; }
    public function hasDivorceDateCal() { return $this->divorceDateCal_ !== null; }
    public function getDivorceDateCal() { if($this->divorceDateCal_ === null) return Calendar::GREGORIAN; else return $this->divorceDateCal_; }
    public function setDivorceDateCal($value) { $this->divorceDateCal_ = $value; }

    // repeated .SimplePerson witnesses = 13;

    private $witnesses_ = null;
    public function clearWitnesses() { $this->witnesses_ = null; }
    public function getWitnessesCount() { if ($this->witnesses_ === null ) return 0; else return count($this->witnesses_); }
    public function getWitnesses($index) { return $this->witnesses_[$index]; }
    public function getWitnessesArray() { if ($this->witnesses_ === null ) return array(); else return $this->witnesses_; }
    public function setWitnesses($index, $value) {$this->witnesses_[$index] = $value;	}
    public function addWitnesses($value) { $this->witnesses_[] = $value; }
    public function addAllWitnesses(array $values) { foreach($values as $value) {$this->witnesses_[] = $value;} }

    // optional string notes = 14;

    private $notes_ = null;
    public function clearNotes() { $this->notes_ = null; }
    public function hasNotes() { return $this->notes_ !== null; }
    public function getNotes() { if($this->notes_ === null) return ""; else return $this->notes_; }
    public function setNotes($value) { $this->notes_ = $value; }

    // optional string fsources = 15;

    private $fsources_ = null;
    public function clearFsources() { $this->fsources_ = null; }
    public function hasFsources() { return $this->fsources_ !== null; }
    public function getFsources() { if($this->fsources_ === null) return ""; else return $this->fsources_; }
    public function setFsources($value) { $this->fsources_ = $value; }

    // repeated .SimplePerson children = 16;

    private $children_ = null;
    public function clearChildren() { $this->children_ = null; }
    public function getChildrenCount() { if ($this->children_ === null ) return 0; else return count($this->children_); }
    public function getChildren($index) { return $this->children_[$index]; }
    public function getChildrenArray() { if ($this->children_ === null ) return array(); else return $this->children_; }
    public function setChildren($index, $value) {$this->children_[$index] = $value;	}
    public function addChildren($value) { $this->children_[] = $value; }
    public function addAllChildren(array $values) { foreach($values as $value) {$this->children_[] = $value;} }

    // @@protoc_insertion_point(class_scope:Family)
  }

  // message IndexPerson
  class IndexPerson {
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
          throw new Exception('Invalid in parameter');
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
        //var_dump("IndexPerson: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->index_ = $tmp;

            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
    }

    function write($fp) {
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
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

    // @@protoc_insertion_point(class_scope:IndexPerson)
  }

  // message Node
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
          throw new Exception('Invalid in parameter');
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
        //var_dump("Node: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->id_ = $tmp;

            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->person_ = new PersonTree($fp, $len);
            ASSERT('$len == 0');
            break;
          case 3:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->ifam_ = $tmp;

            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
    }

    function write($fp) {
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
      if (!is_null($this->id_)) {
        fwrite($fp, "\x08");
        \Protobuf::write_varint($fp, $this->id_);
      }
      if (!is_null($this->person_)) {
        fwrite($fp, "\x12");
        \Protobuf::write_varint($fp, $this->person_->size()); // message
        $this->person_->write($fp);
      }
      if (!is_null($this->ifam_)) {
        fwrite($fp, "\x18");
        \Protobuf::write_varint($fp, $this->ifam_);
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
      if (!is_null($this->ifam_)) {
        $size += 1 + \Protobuf::size_varint($this->ifam_);
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
           . \Protobuf::toString('person_', $this->person_)
           . \Protobuf::toString('ifam_', $this->ifam_);
    }

    // required int64 id = 1;

    private $id_ = null;
    public function clearId() { $this->id_ = null; }
    public function hasId() { return $this->id_ !== null; }
    public function getId() { if($this->id_ === null) return 0; else return $this->id_; }
    public function setId($value) { $this->id_ = $value; }

    // required .PersonTree person = 2;

    private $person_ = null;
    public function clearPerson() { $this->person_ = null; }
    public function hasPerson() { return $this->person_ !== null; }
    public function getPerson() { if($this->person_ === null) return null; else return $this->person_; }
    public function setPerson(PersonTree $value) { $this->person_ = $value; }

    // optional int64 ifam = 3;

    private $ifam_ = null;
    public function clearIfam() { $this->ifam_ = null; }
    public function hasIfam() { return $this->ifam_ !== null; }
    public function getIfam() { if($this->ifam_ === null) return 0; else return $this->ifam_; }
    public function setIfam($value) { $this->ifam_ = $value; }

    // @@protoc_insertion_point(class_scope:Node)
  }

  // message Edge
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
          throw new Exception('Invalid in parameter');
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
        //var_dump("Edge: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->fromNode_ = $tmp;

            break;
          case 2:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->toNode_ = $tmp;

            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
    }

    function write($fp) {
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
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

    // @@protoc_insertion_point(class_scope:Edge)
  }

  // message GraphTree
  class GraphTree {
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
          throw new Exception('Invalid in parameter');
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
        //var_dump("GraphTree: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->nodesAsc_[] = new Node($fp, $len);
            ASSERT('$len == 0');
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->edgesAsc_[] = new Edge($fp, $len);
            ASSERT('$len == 0');
            break;
          case 3:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->nodesDesc_[] = new Node($fp, $len);
            ASSERT('$len == 0');
            break;
          case 4:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->edgesDesc_[] = new Edge($fp, $len);
            ASSERT('$len == 0');
            break;
          case 5:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->nodesSiblings_[] = new Node($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
    }

    function write($fp) {
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
      if (!is_null($this->nodesAsc_))
        foreach($this->nodesAsc_ as $v) {
          fwrite($fp, "\x0a");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->edgesAsc_))
        foreach($this->edgesAsc_ as $v) {
          fwrite($fp, "\x12");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->nodesDesc_))
        foreach($this->nodesDesc_ as $v) {
          fwrite($fp, "\x1a");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->edgesDesc_))
        foreach($this->edgesDesc_ as $v) {
          fwrite($fp, "\"");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->nodesSiblings_))
        foreach($this->nodesSiblings_ as $v) {
          fwrite($fp, "*");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
    }

    public function size() {
      $size = 0;
      if (!is_null($this->nodesAsc_))
        foreach($this->nodesAsc_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->edgesAsc_))
        foreach($this->edgesAsc_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->nodesDesc_))
        foreach($this->nodesDesc_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->edgesDesc_))
        foreach($this->edgesDesc_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->nodesSiblings_))
        foreach($this->nodesSiblings_ as $v) {
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
           . \Protobuf::toString('nodesAsc_', $this->nodesAsc_)
           . \Protobuf::toString('edgesAsc_', $this->edgesAsc_)
           . \Protobuf::toString('nodesDesc_', $this->nodesDesc_)
           . \Protobuf::toString('edgesDesc_', $this->edgesDesc_)
           . \Protobuf::toString('nodesSiblings_', $this->nodesSiblings_);
    }

    // repeated .Node nodes_asc = 1;

    private $nodesAsc_ = null;
    public function clearNodesAsc() { $this->nodesAsc_ = null; }
    public function getNodesAscCount() { if ($this->nodesAsc_ === null ) return 0; else return count($this->nodesAsc_); }
    public function getNodesAsc($index) { return $this->nodesAsc_[$index]; }
    public function getNodesAscArray() { if ($this->nodesAsc_ === null ) return array(); else return $this->nodesAsc_; }
    public function setNodesAsc($index, $value) {$this->nodesAsc_[$index] = $value;	}
    public function addNodesAsc($value) { $this->nodesAsc_[] = $value; }
    public function addAllNodesAsc(array $values) { foreach($values as $value) {$this->nodesAsc_[] = $value;} }

    // repeated .Edge edges_asc = 2;

    private $edgesAsc_ = null;
    public function clearEdgesAsc() { $this->edgesAsc_ = null; }
    public function getEdgesAscCount() { if ($this->edgesAsc_ === null ) return 0; else return count($this->edgesAsc_); }
    public function getEdgesAsc($index) { return $this->edgesAsc_[$index]; }
    public function getEdgesAscArray() { if ($this->edgesAsc_ === null ) return array(); else return $this->edgesAsc_; }
    public function setEdgesAsc($index, $value) {$this->edgesAsc_[$index] = $value;	}
    public function addEdgesAsc($value) { $this->edgesAsc_[] = $value; }
    public function addAllEdgesAsc(array $values) { foreach($values as $value) {$this->edgesAsc_[] = $value;} }

    // repeated .Node nodes_desc = 3;

    private $nodesDesc_ = null;
    public function clearNodesDesc() { $this->nodesDesc_ = null; }
    public function getNodesDescCount() { if ($this->nodesDesc_ === null ) return 0; else return count($this->nodesDesc_); }
    public function getNodesDesc($index) { return $this->nodesDesc_[$index]; }
    public function getNodesDescArray() { if ($this->nodesDesc_ === null ) return array(); else return $this->nodesDesc_; }
    public function setNodesDesc($index, $value) {$this->nodesDesc_[$index] = $value;	}
    public function addNodesDesc($value) { $this->nodesDesc_[] = $value; }
    public function addAllNodesDesc(array $values) { foreach($values as $value) {$this->nodesDesc_[] = $value;} }

    // repeated .Edge edges_desc = 4;

    private $edgesDesc_ = null;
    public function clearEdgesDesc() { $this->edgesDesc_ = null; }
    public function getEdgesDescCount() { if ($this->edgesDesc_ === null ) return 0; else return count($this->edgesDesc_); }
    public function getEdgesDesc($index) { return $this->edgesDesc_[$index]; }
    public function getEdgesDescArray() { if ($this->edgesDesc_ === null ) return array(); else return $this->edgesDesc_; }
    public function setEdgesDesc($index, $value) {$this->edgesDesc_[$index] = $value;	}
    public function addEdgesDesc($value) { $this->edgesDesc_[] = $value; }
    public function addAllEdgesDesc(array $values) { foreach($values as $value) {$this->edgesDesc_[] = $value;} }

    // repeated .Node nodes_siblings = 5;

    private $nodesSiblings_ = null;
    public function clearNodesSiblings() { $this->nodesSiblings_ = null; }
    public function getNodesSiblingsCount() { if ($this->nodesSiblings_ === null ) return 0; else return count($this->nodesSiblings_); }
    public function getNodesSiblings($index) { return $this->nodesSiblings_[$index]; }
    public function getNodesSiblingsArray() { if ($this->nodesSiblings_ === null ) return array(); else return $this->nodesSiblings_; }
    public function setNodesSiblings($index, $value) {$this->nodesSiblings_[$index] = $value;	}
    public function addNodesSiblings($value) { $this->nodesSiblings_[] = $value; }
    public function addAllNodesSiblings(array $values) { foreach($values as $value) {$this->nodesSiblings_[] = $value;} }

    // @@protoc_insertion_point(class_scope:GraphTree)
  }

  // message GraphTreeNew
  class GraphTreeNew {
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
          throw new Exception('Invalid in parameter');
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
        //var_dump("GraphTreeNew: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->nodesAsc_[] = new Node($fp, $len);
            ASSERT('$len == 0');
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->edgesAsc_[] = new Edge($fp, $len);
            ASSERT('$len == 0');
            break;
          case 3:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->nodesDesc_[] = new Node($fp, $len);
            ASSERT('$len == 0');
            break;
          case 4:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->edgesDesc_[] = new Edge($fp, $len);
            ASSERT('$len == 0');
            break;
          case 5:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->nodesSiblings_[] = new Node($fp, $len);
            ASSERT('$len == 0');
            break;
          case 6:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->nodesSiblingsBefore_[] = new Node($fp, $len);
            ASSERT('$len == 0');
            break;
          case 7:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->nodesSiblingsAfter_[] = new Node($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
    }

    function write($fp) {
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
      if (!is_null($this->nodesAsc_))
        foreach($this->nodesAsc_ as $v) {
          fwrite($fp, "\x0a");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->edgesAsc_))
        foreach($this->edgesAsc_ as $v) {
          fwrite($fp, "\x12");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->nodesDesc_))
        foreach($this->nodesDesc_ as $v) {
          fwrite($fp, "\x1a");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->edgesDesc_))
        foreach($this->edgesDesc_ as $v) {
          fwrite($fp, "\"");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->nodesSiblings_))
        foreach($this->nodesSiblings_ as $v) {
          fwrite($fp, "*");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->nodesSiblingsBefore_))
        foreach($this->nodesSiblingsBefore_ as $v) {
          fwrite($fp, "2");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->nodesSiblingsAfter_))
        foreach($this->nodesSiblingsAfter_ as $v) {
          fwrite($fp, ":");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
    }

    public function size() {
      $size = 0;
      if (!is_null($this->nodesAsc_))
        foreach($this->nodesAsc_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->edgesAsc_))
        foreach($this->edgesAsc_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->nodesDesc_))
        foreach($this->nodesDesc_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->edgesDesc_))
        foreach($this->edgesDesc_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->nodesSiblings_))
        foreach($this->nodesSiblings_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->nodesSiblingsBefore_))
        foreach($this->nodesSiblingsBefore_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->nodesSiblingsAfter_))
        foreach($this->nodesSiblingsAfter_ as $v) {
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
           . \Protobuf::toString('nodesAsc_', $this->nodesAsc_)
           . \Protobuf::toString('edgesAsc_', $this->edgesAsc_)
           . \Protobuf::toString('nodesDesc_', $this->nodesDesc_)
           . \Protobuf::toString('edgesDesc_', $this->edgesDesc_)
           . \Protobuf::toString('nodesSiblings_', $this->nodesSiblings_)
           . \Protobuf::toString('nodesSiblingsBefore_', $this->nodesSiblingsBefore_)
           . \Protobuf::toString('nodesSiblingsAfter_', $this->nodesSiblingsAfter_);
    }

    // repeated .Node nodes_asc = 1;

    private $nodesAsc_ = null;
    public function clearNodesAsc() { $this->nodesAsc_ = null; }
    public function getNodesAscCount() { if ($this->nodesAsc_ === null ) return 0; else return count($this->nodesAsc_); }
    public function getNodesAsc($index) { return $this->nodesAsc_[$index]; }
    public function getNodesAscArray() { if ($this->nodesAsc_ === null ) return array(); else return $this->nodesAsc_; }
    public function setNodesAsc($index, $value) {$this->nodesAsc_[$index] = $value;	}
    public function addNodesAsc($value) { $this->nodesAsc_[] = $value; }
    public function addAllNodesAsc(array $values) { foreach($values as $value) {$this->nodesAsc_[] = $value;} }

    // repeated .Edge edges_asc = 2;

    private $edgesAsc_ = null;
    public function clearEdgesAsc() { $this->edgesAsc_ = null; }
    public function getEdgesAscCount() { if ($this->edgesAsc_ === null ) return 0; else return count($this->edgesAsc_); }
    public function getEdgesAsc($index) { return $this->edgesAsc_[$index]; }
    public function getEdgesAscArray() { if ($this->edgesAsc_ === null ) return array(); else return $this->edgesAsc_; }
    public function setEdgesAsc($index, $value) {$this->edgesAsc_[$index] = $value;	}
    public function addEdgesAsc($value) { $this->edgesAsc_[] = $value; }
    public function addAllEdgesAsc(array $values) { foreach($values as $value) {$this->edgesAsc_[] = $value;} }

    // repeated .Node nodes_desc = 3;

    private $nodesDesc_ = null;
    public function clearNodesDesc() { $this->nodesDesc_ = null; }
    public function getNodesDescCount() { if ($this->nodesDesc_ === null ) return 0; else return count($this->nodesDesc_); }
    public function getNodesDesc($index) { return $this->nodesDesc_[$index]; }
    public function getNodesDescArray() { if ($this->nodesDesc_ === null ) return array(); else return $this->nodesDesc_; }
    public function setNodesDesc($index, $value) {$this->nodesDesc_[$index] = $value;	}
    public function addNodesDesc($value) { $this->nodesDesc_[] = $value; }
    public function addAllNodesDesc(array $values) { foreach($values as $value) {$this->nodesDesc_[] = $value;} }

    // repeated .Edge edges_desc = 4;

    private $edgesDesc_ = null;
    public function clearEdgesDesc() { $this->edgesDesc_ = null; }
    public function getEdgesDescCount() { if ($this->edgesDesc_ === null ) return 0; else return count($this->edgesDesc_); }
    public function getEdgesDesc($index) { return $this->edgesDesc_[$index]; }
    public function getEdgesDescArray() { if ($this->edgesDesc_ === null ) return array(); else return $this->edgesDesc_; }
    public function setEdgesDesc($index, $value) {$this->edgesDesc_[$index] = $value;	}
    public function addEdgesDesc($value) { $this->edgesDesc_[] = $value; }
    public function addAllEdgesDesc(array $values) { foreach($values as $value) {$this->edgesDesc_[] = $value;} }

    // repeated .Node nodes_siblings = 5;

    private $nodesSiblings_ = null;
    public function clearNodesSiblings() { $this->nodesSiblings_ = null; }
    public function getNodesSiblingsCount() { if ($this->nodesSiblings_ === null ) return 0; else return count($this->nodesSiblings_); }
    public function getNodesSiblings($index) { return $this->nodesSiblings_[$index]; }
    public function getNodesSiblingsArray() { if ($this->nodesSiblings_ === null ) return array(); else return $this->nodesSiblings_; }
    public function setNodesSiblings($index, $value) {$this->nodesSiblings_[$index] = $value;	}
    public function addNodesSiblings($value) { $this->nodesSiblings_[] = $value; }
    public function addAllNodesSiblings(array $values) { foreach($values as $value) {$this->nodesSiblings_[] = $value;} }

    // repeated .Node nodes_siblings_before = 6;

    private $nodesSiblingsBefore_ = null;
    public function clearNodesSiblingsBefore() { $this->nodesSiblingsBefore_ = null; }
    public function getNodesSiblingsBeforeCount() { if ($this->nodesSiblingsBefore_ === null ) return 0; else return count($this->nodesSiblingsBefore_); }
    public function getNodesSiblingsBefore($index) { return $this->nodesSiblingsBefore_[$index]; }
    public function getNodesSiblingsBeforeArray() { if ($this->nodesSiblingsBefore_ === null ) return array(); else return $this->nodesSiblingsBefore_; }
    public function setNodesSiblingsBefore($index, $value) {$this->nodesSiblingsBefore_[$index] = $value;	}
    public function addNodesSiblingsBefore($value) { $this->nodesSiblingsBefore_[] = $value; }
    public function addAllNodesSiblingsBefore(array $values) { foreach($values as $value) {$this->nodesSiblingsBefore_[] = $value;} }

    // repeated .Node nodes_siblings_after = 7;

    private $nodesSiblingsAfter_ = null;
    public function clearNodesSiblingsAfter() { $this->nodesSiblingsAfter_ = null; }
    public function getNodesSiblingsAfterCount() { if ($this->nodesSiblingsAfter_ === null ) return 0; else return count($this->nodesSiblingsAfter_); }
    public function getNodesSiblingsAfter($index) { return $this->nodesSiblingsAfter_[$index]; }
    public function getNodesSiblingsAfterArray() { if ($this->nodesSiblingsAfter_ === null ) return array(); else return $this->nodesSiblingsAfter_; }
    public function setNodesSiblingsAfter($index, $value) {$this->nodesSiblingsAfter_[$index] = $value;	}
    public function addNodesSiblingsAfter($value) { $this->nodesSiblingsAfter_[] = $value; }
    public function addAllNodesSiblingsAfter(array $values) { foreach($values as $value) {$this->nodesSiblingsAfter_[] = $value;} }

    // @@protoc_insertion_point(class_scope:GraphTreeNew)
  }

  // message GraphTreeParams
  class GraphTreeParams {
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
          throw new Exception('Invalid in parameter');
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
        //var_dump("GraphTreeParams: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->index_ = $tmp;

            break;
          case 2:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->nbAsc_ = $tmp;

            break;
          case 3:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->nbDesc_ = $tmp;

            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
    }

    function write($fp) {
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
      if (!is_null($this->index_)) {
        fwrite($fp, "\x08");
        \Protobuf::write_varint($fp, $this->index_);
      }
      if (!is_null($this->nbAsc_)) {
        fwrite($fp, "\x10");
        \Protobuf::write_varint($fp, $this->nbAsc_);
      }
      if (!is_null($this->nbDesc_)) {
        fwrite($fp, "\x18");
        \Protobuf::write_varint($fp, $this->nbDesc_);
      }
    }

    public function size() {
      $size = 0;
      if (!is_null($this->index_)) {
        $size += 1 + \Protobuf::size_varint($this->index_);
      }
      if (!is_null($this->nbAsc_)) {
        $size += 1 + \Protobuf::size_varint($this->nbAsc_);
      }
      if (!is_null($this->nbDesc_)) {
        $size += 1 + \Protobuf::size_varint($this->nbDesc_);
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
           . \Protobuf::toString('index_', $this->index_)
           . \Protobuf::toString('nbAsc_', $this->nbAsc_)
           . \Protobuf::toString('nbDesc_', $this->nbDesc_);
    }

    // required int32 index = 1;

    private $index_ = null;
    public function clearIndex() { $this->index_ = null; }
    public function hasIndex() { return $this->index_ !== null; }
    public function getIndex() { if($this->index_ === null) return 0; else return $this->index_; }
    public function setIndex($value) { $this->index_ = $value; }

    // optional int32 nb_asc = 2;

    private $nbAsc_ = null;
    public function clearNbAsc() { $this->nbAsc_ = null; }
    public function hasNbAsc() { return $this->nbAsc_ !== null; }
    public function getNbAsc() { if($this->nbAsc_ === null) return 0; else return $this->nbAsc_; }
    public function setNbAsc($value) { $this->nbAsc_ = $value; }

    // optional int32 nb_desc = 3;

    private $nbDesc_ = null;
    public function clearNbDesc() { $this->nbDesc_ = null; }
    public function hasNbDesc() { return $this->nbDesc_ !== null; }
    public function getNbDesc() { if($this->nbDesc_ === null) return 0; else return $this->nbDesc_; }
    public function setNbDesc($value) { $this->nbDesc_ = $value; }

    // @@protoc_insertion_point(class_scope:GraphTreeParams)
  }

  // message Title
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
          throw new Exception('Invalid in parameter');
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
        //var_dump("Title: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->titleType_ = $tmp;

            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->name_ = $tmp;
            $limit-=$len;
            break;
          case 3:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->title_ = $tmp;
            $limit-=$len;
            break;
          case 4:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->fief_ = $tmp;
            $limit-=$len;
            break;
          case 5:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->dateBegin_ = $tmp;
            $limit-=$len;
            break;
          case 6:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->dateEnd_ = $tmp;
            $limit-=$len;
            break;
          case 7:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->nth_ = $tmp;

            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
    }

    function write($fp) {
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
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
           . \Protobuf::toString('titleType_', TitleType::toString($this->titleType_))
           . \Protobuf::toString('name_', $this->name_)
           . \Protobuf::toString('title_', $this->title_)
           . \Protobuf::toString('fief_', $this->fief_)
           . \Protobuf::toString('dateBegin_', $this->dateBegin_)
           . \Protobuf::toString('dateEnd_', $this->dateEnd_)
           . \Protobuf::toString('nth_', $this->nth_);
    }

    // required .TitleType title_type = 1;

    private $titleType_ = null;
    public function clearTitleType() { $this->titleType_ = null; }
    public function hasTitleType() { return $this->titleType_ !== null; }
    public function getTitleType() { if($this->titleType_ === null) return TitleType::TITLE_MAIN; else return $this->titleType_; }
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

    // @@protoc_insertion_point(class_scope:Title)
  }

  // message PersonTreeFull
  class PersonTreeFull {
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
          throw new Exception('Invalid in parameter');
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
        //var_dump("PersonTreeFull: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->index_ = $tmp;

            break;
          case 2:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->sex_ = $tmp;

            break;
          case 3:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->lastname_ = $tmp;
            $limit-=$len;
            break;
          case 4:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->firstname_ = $tmp;
            $limit-=$len;
            break;
          case 5:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->n_ = $tmp;
            $limit-=$len;
            break;
          case 6:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->p_ = $tmp;
            $limit-=$len;
            break;
          case 7:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->occ_ = $tmp;

            break;
          case 8:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->image_ = $tmp;
            $limit-=$len;
            break;
          case 9:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->sosa_ = $tmp;

            break;
          case 10:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->publicName_ = $tmp;
            $limit-=$len;
            break;
          case 11:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->aliases_[] = $tmp;
            $limit-=$len;
            break;
          case 12:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->qualifiers_[] = $tmp;
            $limit-=$len;
            break;
          case 13:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->firstnameAliases_[] = $tmp;
            $limit-=$len;
            break;
          case 14:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->surnameAliases_[] = $tmp;
            $limit-=$len;
            break;
          case 15:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->birthDate_ = $tmp;
            $limit-=$len;
            break;
          case 16:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->birthPlace_ = $tmp;
            $limit-=$len;
            break;
          case 17:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->birthSrc_ = $tmp;
            $limit-=$len;
            break;
          case 18:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->baptismDate_ = $tmp;
            $limit-=$len;
            break;
          case 19:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->baptismPlace_ = $tmp;
            $limit-=$len;
            break;
          case 20:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->baptismSrc_ = $tmp;
            $limit-=$len;
            break;
          case 21:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->deathDate_ = $tmp;
            $limit-=$len;
            break;
          case 22:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->deathPlace_ = $tmp;
            $limit-=$len;
            break;
          case 23:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->deathSrc_ = $tmp;
            $limit-=$len;
            break;
          case 24:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->deathType_ = $tmp;

            break;
          case 25:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->burialDate_ = $tmp;
            $limit-=$len;
            break;
          case 26:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->burialPlace_ = $tmp;
            $limit-=$len;
            break;
          case 27:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->burialSrc_ = $tmp;
            $limit-=$len;
            break;
          case 28:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->occupation_ = $tmp;
            $limit-=$len;
            break;
          case 29:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->psources_ = $tmp;
            $limit-=$len;
            break;
          case 30:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->titles_[] = new Title($fp, $len);
            ASSERT('$len == 0');
            break;
          case 31:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->visibleForVisitors_ = $tmp > 0 ? true : false;
            break;
          case 32:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->hasMoreInfos_ = $tmp > 0 ? true : false;
            break;
          case 33:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->baseprefix_ = $tmp;
            $limit-=$len;
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
    }

    function write($fp) {
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
      if (!is_null($this->index_)) {
        fwrite($fp, "\x08");
        \Protobuf::write_varint($fp, $this->index_);
      }
      if (!is_null($this->sex_)) {
        fwrite($fp, "\x10");
        \Protobuf::write_varint($fp, $this->sex_);
      }
      if (!is_null($this->lastname_)) {
        fwrite($fp, "\x1a");
        \Protobuf::write_varint($fp, strlen($this->lastname_));
        fwrite($fp, $this->lastname_);
      }
      if (!is_null($this->firstname_)) {
        fwrite($fp, "\"");
        \Protobuf::write_varint($fp, strlen($this->firstname_));
        fwrite($fp, $this->firstname_);
      }
      if (!is_null($this->n_)) {
        fwrite($fp, "*");
        \Protobuf::write_varint($fp, strlen($this->n_));
        fwrite($fp, $this->n_);
      }
      if (!is_null($this->p_)) {
        fwrite($fp, "2");
        \Protobuf::write_varint($fp, strlen($this->p_));
        fwrite($fp, $this->p_);
      }
      if (!is_null($this->occ_)) {
        fwrite($fp, "8");
        \Protobuf::write_varint($fp, $this->occ_);
      }
      if (!is_null($this->image_)) {
        fwrite($fp, "B");
        \Protobuf::write_varint($fp, strlen($this->image_));
        fwrite($fp, $this->image_);
      }
      if (!is_null($this->sosa_)) {
        fwrite($fp, "H");
        \Protobuf::write_varint($fp, $this->sosa_);
      }
      if (!is_null($this->publicName_)) {
        fwrite($fp, "R");
        \Protobuf::write_varint($fp, strlen($this->publicName_));
        fwrite($fp, $this->publicName_);
      }
      if (!is_null($this->aliases_))
        foreach($this->aliases_ as $v) {
          fwrite($fp, "Z");
          \Protobuf::write_varint($fp, strlen($v));
          fwrite($fp, $v);
        }
      if (!is_null($this->qualifiers_))
        foreach($this->qualifiers_ as $v) {
          fwrite($fp, "b");
          \Protobuf::write_varint($fp, strlen($v));
          fwrite($fp, $v);
        }
      if (!is_null($this->firstnameAliases_))
        foreach($this->firstnameAliases_ as $v) {
          fwrite($fp, "j");
          \Protobuf::write_varint($fp, strlen($v));
          fwrite($fp, $v);
        }
      if (!is_null($this->surnameAliases_))
        foreach($this->surnameAliases_ as $v) {
          fwrite($fp, "r");
          \Protobuf::write_varint($fp, strlen($v));
          fwrite($fp, $v);
        }
      if (!is_null($this->birthDate_)) {
        fwrite($fp, "z");
        \Protobuf::write_varint($fp, strlen($this->birthDate_));
        fwrite($fp, $this->birthDate_);
      }
      if (!is_null($this->birthPlace_)) {
        fwrite($fp, "\x82\x01");
        \Protobuf::write_varint($fp, strlen($this->birthPlace_));
        fwrite($fp, $this->birthPlace_);
      }
      if (!is_null($this->birthSrc_)) {
        fwrite($fp, "\x8a\x01");
        \Protobuf::write_varint($fp, strlen($this->birthSrc_));
        fwrite($fp, $this->birthSrc_);
      }
      if (!is_null($this->baptismDate_)) {
        fwrite($fp, "\x92\x01");
        \Protobuf::write_varint($fp, strlen($this->baptismDate_));
        fwrite($fp, $this->baptismDate_);
      }
      if (!is_null($this->baptismPlace_)) {
        fwrite($fp, "\x9a\x01");
        \Protobuf::write_varint($fp, strlen($this->baptismPlace_));
        fwrite($fp, $this->baptismPlace_);
      }
      if (!is_null($this->baptismSrc_)) {
        fwrite($fp, "\xa2\x01");
        \Protobuf::write_varint($fp, strlen($this->baptismSrc_));
        fwrite($fp, $this->baptismSrc_);
      }
      if (!is_null($this->deathDate_)) {
        fwrite($fp, "\xaa\x01");
        \Protobuf::write_varint($fp, strlen($this->deathDate_));
        fwrite($fp, $this->deathDate_);
      }
      if (!is_null($this->deathPlace_)) {
        fwrite($fp, "\xb2\x01");
        \Protobuf::write_varint($fp, strlen($this->deathPlace_));
        fwrite($fp, $this->deathPlace_);
      }
      if (!is_null($this->deathSrc_)) {
        fwrite($fp, "\xba\x01");
        \Protobuf::write_varint($fp, strlen($this->deathSrc_));
        fwrite($fp, $this->deathSrc_);
      }
      if (!is_null($this->deathType_)) {
        fwrite($fp, "\xc0\x01");
        \Protobuf::write_varint($fp, $this->deathType_);
      }
      if (!is_null($this->burialDate_)) {
        fwrite($fp, "\xca\x01");
        \Protobuf::write_varint($fp, strlen($this->burialDate_));
        fwrite($fp, $this->burialDate_);
      }
      if (!is_null($this->burialPlace_)) {
        fwrite($fp, "\xd2\x01");
        \Protobuf::write_varint($fp, strlen($this->burialPlace_));
        fwrite($fp, $this->burialPlace_);
      }
      if (!is_null($this->burialSrc_)) {
        fwrite($fp, "\xda\x01");
        \Protobuf::write_varint($fp, strlen($this->burialSrc_));
        fwrite($fp, $this->burialSrc_);
      }
      if (!is_null($this->occupation_)) {
        fwrite($fp, "\xe2\x01");
        \Protobuf::write_varint($fp, strlen($this->occupation_));
        fwrite($fp, $this->occupation_);
      }
      if (!is_null($this->psources_)) {
        fwrite($fp, "\xea\x01");
        \Protobuf::write_varint($fp, strlen($this->psources_));
        fwrite($fp, $this->psources_);
      }
      if (!is_null($this->titles_))
        foreach($this->titles_ as $v) {
          fwrite($fp, "\xf2\x01");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->visibleForVisitors_)) {
        fwrite($fp, "\xf8\x01");
        \Protobuf::write_varint($fp, $this->visibleForVisitors_ ? 1 : 0);
      }
      if (!is_null($this->hasMoreInfos_)) {
        fwrite($fp, "\x80\x02");
        \Protobuf::write_varint($fp, $this->hasMoreInfos_ ? 1 : 0);
      }
      if (!is_null($this->baseprefix_)) {
        fwrite($fp, "\x8a\x02");
        \Protobuf::write_varint($fp, strlen($this->baseprefix_));
        fwrite($fp, $this->baseprefix_);
      }
    }

    public function size() {
      $size = 0;
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
      if (!is_null($this->n_)) {
        $l = strlen($this->n_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->p_)) {
        $l = strlen($this->p_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->occ_)) {
        $size += 1 + \Protobuf::size_varint($this->occ_);
      }
      if (!is_null($this->image_)) {
        $l = strlen($this->image_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
      }
      if (!is_null($this->sosa_)) {
        $size += 1 + \Protobuf::size_varint($this->sosa_);
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
      if (!is_null($this->birthDate_)) {
        $l = strlen($this->birthDate_);
        $size += 1 + \Protobuf::size_varint($l) + $l;
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
      if (!is_null($this->visibleForVisitors_)) {
        $size += 3;
      }
      if (!is_null($this->hasMoreInfos_)) {
        $size += 3;
      }
      if (!is_null($this->baseprefix_)) {
        $l = strlen($this->baseprefix_);
        $size += 2 + \Protobuf::size_varint($l) + $l;
      }
      return $size;
    }

    public function validateRequired() {
      if ($this->index_ === null) return false;
      if ($this->sex_ === null) return false;
      if ($this->lastname_ === null) return false;
      if ($this->firstname_ === null) return false;
      if ($this->n_ === null) return false;
      if ($this->p_ === null) return false;
      if ($this->occ_ === null) return false;
      if ($this->sosa_ === null) return false;
      if ($this->deathType_ === null) return false;
      if ($this->visibleForVisitors_ === null) return false;
      if ($this->hasMoreInfos_ === null) return false;
      if ($this->baseprefix_ === null) return false;
      return true;
    }

    public function __toString() {
      return ''
           . \Protobuf::toString('unknown', $this->_unknown)
           . \Protobuf::toString('index_', $this->index_)
           . \Protobuf::toString('sex_', Sex::toString($this->sex_))
           . \Protobuf::toString('lastname_', $this->lastname_)
           . \Protobuf::toString('firstname_', $this->firstname_)
           . \Protobuf::toString('n_', $this->n_)
           . \Protobuf::toString('p_', $this->p_)
           . \Protobuf::toString('occ_', $this->occ_)
           . \Protobuf::toString('image_', $this->image_)
           . \Protobuf::toString('sosa_', Sosa::toString($this->sosa_))
           . \Protobuf::toString('publicName_', $this->publicName_)
           . \Protobuf::toString('aliases_', $this->aliases_)
           . \Protobuf::toString('qualifiers_', $this->qualifiers_)
           . \Protobuf::toString('firstnameAliases_', $this->firstnameAliases_)
           . \Protobuf::toString('surnameAliases_', $this->surnameAliases_)
           . \Protobuf::toString('birthDate_', $this->birthDate_)
           . \Protobuf::toString('birthPlace_', $this->birthPlace_)
           . \Protobuf::toString('birthSrc_', $this->birthSrc_)
           . \Protobuf::toString('baptismDate_', $this->baptismDate_)
           . \Protobuf::toString('baptismPlace_', $this->baptismPlace_)
           . \Protobuf::toString('baptismSrc_', $this->baptismSrc_)
           . \Protobuf::toString('deathDate_', $this->deathDate_)
           . \Protobuf::toString('deathPlace_', $this->deathPlace_)
           . \Protobuf::toString('deathSrc_', $this->deathSrc_)
           . \Protobuf::toString('deathType_', DeathType::toString($this->deathType_))
           . \Protobuf::toString('burialDate_', $this->burialDate_)
           . \Protobuf::toString('burialPlace_', $this->burialPlace_)
           . \Protobuf::toString('burialSrc_', $this->burialSrc_)
           . \Protobuf::toString('occupation_', $this->occupation_)
           . \Protobuf::toString('psources_', $this->psources_)
           . \Protobuf::toString('titles_', $this->titles_)
           . \Protobuf::toString('visibleForVisitors_', $this->visibleForVisitors_)
           . \Protobuf::toString('hasMoreInfos_', $this->hasMoreInfos_)
           . \Protobuf::toString('baseprefix_', $this->baseprefix_);
    }

    // required int32 index = 1;

    private $index_ = null;
    public function clearIndex() { $this->index_ = null; }
    public function hasIndex() { return $this->index_ !== null; }
    public function getIndex() { if($this->index_ === null) return 0; else return $this->index_; }
    public function setIndex($value) { $this->index_ = $value; }

    // required .Sex sex = 2;

    private $sex_ = null;
    public function clearSex() { $this->sex_ = null; }
    public function hasSex() { return $this->sex_ !== null; }
    public function getSex() { if($this->sex_ === null) return Sex::MALE; else return $this->sex_; }
    public function setSex($value) { $this->sex_ = $value; }

    // required string lastname = 3;

    private $lastname_ = null;
    public function clearLastname() { $this->lastname_ = null; }
    public function hasLastname() { return $this->lastname_ !== null; }
    public function getLastname() { if($this->lastname_ === null) return ""; else return $this->lastname_; }
    public function setLastname($value) { $this->lastname_ = $value; }

    // required string firstname = 4;

    private $firstname_ = null;
    public function clearFirstname() { $this->firstname_ = null; }
    public function hasFirstname() { return $this->firstname_ !== null; }
    public function getFirstname() { if($this->firstname_ === null) return ""; else return $this->firstname_; }
    public function setFirstname($value) { $this->firstname_ = $value; }

    // required string n = 5;

    private $n_ = null;
    public function clearN() { $this->n_ = null; }
    public function hasN() { return $this->n_ !== null; }
    public function getN() { if($this->n_ === null) return ""; else return $this->n_; }
    public function setN($value) { $this->n_ = $value; }

    // required string p = 6;

    private $p_ = null;
    public function clearP() { $this->p_ = null; }
    public function hasP() { return $this->p_ !== null; }
    public function getP() { if($this->p_ === null) return ""; else return $this->p_; }
    public function setP($value) { $this->p_ = $value; }

    // required int32 occ = 7;

    private $occ_ = null;
    public function clearOcc() { $this->occ_ = null; }
    public function hasOcc() { return $this->occ_ !== null; }
    public function getOcc() { if($this->occ_ === null) return 0; else return $this->occ_; }
    public function setOcc($value) { $this->occ_ = $value; }

    // optional string image = 8;

    private $image_ = null;
    public function clearImage() { $this->image_ = null; }
    public function hasImage() { return $this->image_ !== null; }
    public function getImage() { if($this->image_ === null) return ""; else return $this->image_; }
    public function setImage($value) { $this->image_ = $value; }

    // required .Sosa sosa = 9;

    private $sosa_ = null;
    public function clearSosa() { $this->sosa_ = null; }
    public function hasSosa() { return $this->sosa_ !== null; }
    public function getSosa() { if($this->sosa_ === null) return Sosa::SOSA_REF; else return $this->sosa_; }
    public function setSosa($value) { $this->sosa_ = $value; }

    // optional string public_name = 10;

    private $publicName_ = null;
    public function clearPublicName() { $this->publicName_ = null; }
    public function hasPublicName() { return $this->publicName_ !== null; }
    public function getPublicName() { if($this->publicName_ === null) return ""; else return $this->publicName_; }
    public function setPublicName($value) { $this->publicName_ = $value; }

    // repeated string aliases = 11;

    private $aliases_ = null;
    public function clearAliases() { $this->aliases_ = null; }
    public function getAliasesCount() { if ($this->aliases_ === null ) return 0; else return count($this->aliases_); }
    public function getAliases($index) { return $this->aliases_[$index]; }
    public function getAliasesArray() { if ($this->aliases_ === null ) return array(); else return $this->aliases_; }
    public function setAliases($index, $value) {$this->aliases_[$index] = $value;	}
    public function addAliases($value) { $this->aliases_[] = $value; }
    public function addAllAliases(array $values) { foreach($values as $value) {$this->aliases_[] = $value;} }

    // repeated string qualifiers = 12;

    private $qualifiers_ = null;
    public function clearQualifiers() { $this->qualifiers_ = null; }
    public function getQualifiersCount() { if ($this->qualifiers_ === null ) return 0; else return count($this->qualifiers_); }
    public function getQualifiers($index) { return $this->qualifiers_[$index]; }
    public function getQualifiersArray() { if ($this->qualifiers_ === null ) return array(); else return $this->qualifiers_; }
    public function setQualifiers($index, $value) {$this->qualifiers_[$index] = $value;	}
    public function addQualifiers($value) { $this->qualifiers_[] = $value; }
    public function addAllQualifiers(array $values) { foreach($values as $value) {$this->qualifiers_[] = $value;} }

    // repeated string firstname_aliases = 13;

    private $firstnameAliases_ = null;
    public function clearFirstnameAliases() { $this->firstnameAliases_ = null; }
    public function getFirstnameAliasesCount() { if ($this->firstnameAliases_ === null ) return 0; else return count($this->firstnameAliases_); }
    public function getFirstnameAliases($index) { return $this->firstnameAliases_[$index]; }
    public function getFirstnameAliasesArray() { if ($this->firstnameAliases_ === null ) return array(); else return $this->firstnameAliases_; }
    public function setFirstnameAliases($index, $value) {$this->firstnameAliases_[$index] = $value;	}
    public function addFirstnameAliases($value) { $this->firstnameAliases_[] = $value; }
    public function addAllFirstnameAliases(array $values) { foreach($values as $value) {$this->firstnameAliases_[] = $value;} }

    // repeated string surname_aliases = 14;

    private $surnameAliases_ = null;
    public function clearSurnameAliases() { $this->surnameAliases_ = null; }
    public function getSurnameAliasesCount() { if ($this->surnameAliases_ === null ) return 0; else return count($this->surnameAliases_); }
    public function getSurnameAliases($index) { return $this->surnameAliases_[$index]; }
    public function getSurnameAliasesArray() { if ($this->surnameAliases_ === null ) return array(); else return $this->surnameAliases_; }
    public function setSurnameAliases($index, $value) {$this->surnameAliases_[$index] = $value;	}
    public function addSurnameAliases($value) { $this->surnameAliases_[] = $value; }
    public function addAllSurnameAliases(array $values) { foreach($values as $value) {$this->surnameAliases_[] = $value;} }

    // optional string birth_date = 15;

    private $birthDate_ = null;
    public function clearBirthDate() { $this->birthDate_ = null; }
    public function hasBirthDate() { return $this->birthDate_ !== null; }
    public function getBirthDate() { if($this->birthDate_ === null) return ""; else return $this->birthDate_; }
    public function setBirthDate($value) { $this->birthDate_ = $value; }

    // optional string birth_place = 16;

    private $birthPlace_ = null;
    public function clearBirthPlace() { $this->birthPlace_ = null; }
    public function hasBirthPlace() { return $this->birthPlace_ !== null; }
    public function getBirthPlace() { if($this->birthPlace_ === null) return ""; else return $this->birthPlace_; }
    public function setBirthPlace($value) { $this->birthPlace_ = $value; }

    // optional string birth_src = 17;

    private $birthSrc_ = null;
    public function clearBirthSrc() { $this->birthSrc_ = null; }
    public function hasBirthSrc() { return $this->birthSrc_ !== null; }
    public function getBirthSrc() { if($this->birthSrc_ === null) return ""; else return $this->birthSrc_; }
    public function setBirthSrc($value) { $this->birthSrc_ = $value; }

    // optional string baptism_date = 18;

    private $baptismDate_ = null;
    public function clearBaptismDate() { $this->baptismDate_ = null; }
    public function hasBaptismDate() { return $this->baptismDate_ !== null; }
    public function getBaptismDate() { if($this->baptismDate_ === null) return ""; else return $this->baptismDate_; }
    public function setBaptismDate($value) { $this->baptismDate_ = $value; }

    // optional string baptism_place = 19;

    private $baptismPlace_ = null;
    public function clearBaptismPlace() { $this->baptismPlace_ = null; }
    public function hasBaptismPlace() { return $this->baptismPlace_ !== null; }
    public function getBaptismPlace() { if($this->baptismPlace_ === null) return ""; else return $this->baptismPlace_; }
    public function setBaptismPlace($value) { $this->baptismPlace_ = $value; }

    // optional string baptism_src = 20;

    private $baptismSrc_ = null;
    public function clearBaptismSrc() { $this->baptismSrc_ = null; }
    public function hasBaptismSrc() { return $this->baptismSrc_ !== null; }
    public function getBaptismSrc() { if($this->baptismSrc_ === null) return ""; else return $this->baptismSrc_; }
    public function setBaptismSrc($value) { $this->baptismSrc_ = $value; }

    // optional string death_date = 21;

    private $deathDate_ = null;
    public function clearDeathDate() { $this->deathDate_ = null; }
    public function hasDeathDate() { return $this->deathDate_ !== null; }
    public function getDeathDate() { if($this->deathDate_ === null) return ""; else return $this->deathDate_; }
    public function setDeathDate($value) { $this->deathDate_ = $value; }

    // optional string death_place = 22;

    private $deathPlace_ = null;
    public function clearDeathPlace() { $this->deathPlace_ = null; }
    public function hasDeathPlace() { return $this->deathPlace_ !== null; }
    public function getDeathPlace() { if($this->deathPlace_ === null) return ""; else return $this->deathPlace_; }
    public function setDeathPlace($value) { $this->deathPlace_ = $value; }

    // optional string death_src = 23;

    private $deathSrc_ = null;
    public function clearDeathSrc() { $this->deathSrc_ = null; }
    public function hasDeathSrc() { return $this->deathSrc_ !== null; }
    public function getDeathSrc() { if($this->deathSrc_ === null) return ""; else return $this->deathSrc_; }
    public function setDeathSrc($value) { $this->deathSrc_ = $value; }

    // required .DeathType death_type = 24;

    private $deathType_ = null;
    public function clearDeathType() { $this->deathType_ = null; }
    public function hasDeathType() { return $this->deathType_ !== null; }
    public function getDeathType() { if($this->deathType_ === null) return DeathType::NOT_DEAD; else return $this->deathType_; }
    public function setDeathType($value) { $this->deathType_ = $value; }

    // optional string burial_date = 25;

    private $burialDate_ = null;
    public function clearBurialDate() { $this->burialDate_ = null; }
    public function hasBurialDate() { return $this->burialDate_ !== null; }
    public function getBurialDate() { if($this->burialDate_ === null) return ""; else return $this->burialDate_; }
    public function setBurialDate($value) { $this->burialDate_ = $value; }

    // optional string burial_place = 26;

    private $burialPlace_ = null;
    public function clearBurialPlace() { $this->burialPlace_ = null; }
    public function hasBurialPlace() { return $this->burialPlace_ !== null; }
    public function getBurialPlace() { if($this->burialPlace_ === null) return ""; else return $this->burialPlace_; }
    public function setBurialPlace($value) { $this->burialPlace_ = $value; }

    // optional string burial_src = 27;

    private $burialSrc_ = null;
    public function clearBurialSrc() { $this->burialSrc_ = null; }
    public function hasBurialSrc() { return $this->burialSrc_ !== null; }
    public function getBurialSrc() { if($this->burialSrc_ === null) return ""; else return $this->burialSrc_; }
    public function setBurialSrc($value) { $this->burialSrc_ = $value; }

    // optional string occupation = 28;

    private $occupation_ = null;
    public function clearOccupation() { $this->occupation_ = null; }
    public function hasOccupation() { return $this->occupation_ !== null; }
    public function getOccupation() { if($this->occupation_ === null) return ""; else return $this->occupation_; }
    public function setOccupation($value) { $this->occupation_ = $value; }

    // optional string psources = 29;

    private $psources_ = null;
    public function clearPsources() { $this->psources_ = null; }
    public function hasPsources() { return $this->psources_ !== null; }
    public function getPsources() { if($this->psources_ === null) return ""; else return $this->psources_; }
    public function setPsources($value) { $this->psources_ = $value; }

    // repeated .Title titles = 30;

    private $titles_ = null;
    public function clearTitles() { $this->titles_ = null; }
    public function getTitlesCount() { if ($this->titles_ === null ) return 0; else return count($this->titles_); }
    public function getTitles($index) { return $this->titles_[$index]; }
    public function getTitlesArray() { if ($this->titles_ === null ) return array(); else return $this->titles_; }
    public function setTitles($index, $value) {$this->titles_[$index] = $value;	}
    public function addTitles($value) { $this->titles_[] = $value; }
    public function addAllTitles(array $values) { foreach($values as $value) {$this->titles_[] = $value;} }

    // required bool visible_for_visitors = 31;

    private $visibleForVisitors_ = null;
    public function clearVisibleForVisitors() { $this->visibleForVisitors_ = null; }
    public function hasVisibleForVisitors() { return $this->visibleForVisitors_ !== null; }
    public function getVisibleForVisitors() { if($this->visibleForVisitors_ === null) return false; else return $this->visibleForVisitors_; }
    public function setVisibleForVisitors($value) { $this->visibleForVisitors_ = $value; }

    // required bool has_more_infos = 32;

    private $hasMoreInfos_ = null;
    public function clearHasMoreInfos() { $this->hasMoreInfos_ = null; }
    public function hasHasMoreInfos() { return $this->hasMoreInfos_ !== null; }
    public function getHasMoreInfos() { if($this->hasMoreInfos_ === null) return false; else return $this->hasMoreInfos_; }
    public function setHasMoreInfos($value) { $this->hasMoreInfos_ = $value; }

    // required string baseprefix = 33;

    private $baseprefix_ = null;
    public function clearBaseprefix() { $this->baseprefix_ = null; }
    public function hasBaseprefix() { return $this->baseprefix_ !== null; }
    public function getBaseprefix() { if($this->baseprefix_ === null) return ""; else return $this->baseprefix_; }
    public function setBaseprefix($value) { $this->baseprefix_ = $value; }

    // @@protoc_insertion_point(class_scope:PersonTreeFull)
  }

  // message FamilyTreeFull
  class FamilyTreeFull {
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
          throw new Exception('Invalid in parameter');
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
        //var_dump("FamilyTreeFull: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->fsources_ = $tmp;
            $limit-=$len;
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->marriageDate_ = $tmp;
            $limit-=$len;
            break;
          case 3:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->marriagePlace_ = $tmp;
            $limit-=$len;
            break;
          case 4:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->marriageSrc_ = $tmp;
            $limit-=$len;
            break;
          case 5:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->marriageType_ = $tmp;

            break;
          case 6:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->divorceType_ = $tmp;

            break;
          case 7:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            if ($len > 0)
              $tmp = fread($fp, $len);
            else
              $tmp = '';
            if ($tmp === false)
              throw new Exception("fread($len) returned false");
            $this->divorceDate_ = $tmp;
            $limit-=$len;
            break;
          case 8:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->index_ = $tmp;

            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
    }

    function write($fp) {
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
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
      if (!is_null($this->index_)) {
        fwrite($fp, "@");
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
      if (!is_null($this->index_)) {
        $size += 1 + \Protobuf::size_varint($this->index_);
      }
      return $size;
    }

    public function validateRequired() {
      if ($this->marriageType_ === null) return false;
      if ($this->divorceType_ === null) return false;
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
           . \Protobuf::toString('marriageType_', MarriageType::toString($this->marriageType_))
           . \Protobuf::toString('divorceType_', DivorceType::toString($this->divorceType_))
           . \Protobuf::toString('divorceDate_', $this->divorceDate_)
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

    // required .MarriageType marriage_type = 5;

    private $marriageType_ = null;
    public function clearMarriageType() { $this->marriageType_ = null; }
    public function hasMarriageType() { return $this->marriageType_ !== null; }
    public function getMarriageType() { if($this->marriageType_ === null) return MarriageType::MARRIED; else return $this->marriageType_; }
    public function setMarriageType($value) { $this->marriageType_ = $value; }

    // required .DivorceType divorce_type = 6;

    private $divorceType_ = null;
    public function clearDivorceType() { $this->divorceType_ = null; }
    public function hasDivorceType() { return $this->divorceType_ !== null; }
    public function getDivorceType() { if($this->divorceType_ === null) return DivorceType::NOT_DIVORCED; else return $this->divorceType_; }
    public function setDivorceType($value) { $this->divorceType_ = $value; }

    // optional string divorce_date = 7;

    private $divorceDate_ = null;
    public function clearDivorceDate() { $this->divorceDate_ = null; }
    public function hasDivorceDate() { return $this->divorceDate_ !== null; }
    public function getDivorceDate() { if($this->divorceDate_ === null) return ""; else return $this->divorceDate_; }
    public function setDivorceDate($value) { $this->divorceDate_ = $value; }

    // required int32 index = 8;

    private $index_ = null;
    public function clearIndex() { $this->index_ = null; }
    public function hasIndex() { return $this->index_ !== null; }
    public function getIndex() { if($this->index_ === null) return 0; else return $this->index_; }
    public function setIndex($value) { $this->index_ = $value; }

    // @@protoc_insertion_point(class_scope:FamilyTreeFull)
  }

  // message NodeFull
  class NodeFull {
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
          throw new Exception('Invalid in parameter');
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
        //var_dump("NodeFull: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->id_ = $tmp;

            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->person_ = new PersonTreeFull($fp, $len);
            ASSERT('$len == 0');
            break;
          case 3:
            ASSERT('$wire == 0');
            $tmp = \Protobuf::read_varint($fp, $limit);
            if ($tmp === false)
              throw new Exception('Protobuf::read_varint returned false');
            $this->ifam_ = $tmp;

            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
    }

    function write($fp) {
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
      if (!is_null($this->id_)) {
        fwrite($fp, "\x08");
        \Protobuf::write_varint($fp, $this->id_);
      }
      if (!is_null($this->person_)) {
        fwrite($fp, "\x12");
        \Protobuf::write_varint($fp, $this->person_->size()); // message
        $this->person_->write($fp);
      }
      if (!is_null($this->ifam_)) {
        fwrite($fp, "\x18");
        \Protobuf::write_varint($fp, $this->ifam_);
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
      if (!is_null($this->ifam_)) {
        $size += 1 + \Protobuf::size_varint($this->ifam_);
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
           . \Protobuf::toString('person_', $this->person_)
           . \Protobuf::toString('ifam_', $this->ifam_);
    }

    // required int64 id = 1;

    private $id_ = null;
    public function clearId() { $this->id_ = null; }
    public function hasId() { return $this->id_ !== null; }
    public function getId() { if($this->id_ === null) return 0; else return $this->id_; }
    public function setId($value) { $this->id_ = $value; }

    // required .PersonTreeFull person = 2;

    private $person_ = null;
    public function clearPerson() { $this->person_ = null; }
    public function hasPerson() { return $this->person_ !== null; }
    public function getPerson() { if($this->person_ === null) return null; else return $this->person_; }
    public function setPerson(PersonTreeFull $value) { $this->person_ = $value; }

    // optional int64 ifam = 3;

    private $ifam_ = null;
    public function clearIfam() { $this->ifam_ = null; }
    public function hasIfam() { return $this->ifam_ !== null; }
    public function getIfam() { if($this->ifam_ === null) return 0; else return $this->ifam_; }
    public function setIfam($value) { $this->ifam_ = $value; }

    // @@protoc_insertion_point(class_scope:NodeFull)
  }

  // message GraphTreeFull
  class GraphTreeFull {
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
          throw new Exception('Invalid in parameter');
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
        //var_dump("GraphTreeFull: Found $field type " . \Protobuf::get_wiretype($wire) . " $limit bytes left");
        switch($field) {
          case 1:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->nodesAsc_[] = new NodeFull($fp, $len);
            ASSERT('$len == 0');
            break;
          case 2:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->edgesAsc_[] = new Edge($fp, $len);
            ASSERT('$len == 0');
            break;
          case 3:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->familiesAsc_[] = new FamilyTreeFull($fp, $len);
            ASSERT('$len == 0');
            break;
          case 4:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->nodesDesc_[] = new NodeFull($fp, $len);
            ASSERT('$len == 0');
            break;
          case 5:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->edgesDesc_[] = new Edge($fp, $len);
            ASSERT('$len == 0');
            break;
          case 6:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->familiesDesc_[] = new FamilyTreeFull($fp, $len);
            ASSERT('$len == 0');
            break;
          case 7:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->nodesSiblings_[] = new NodeFull($fp, $len);
            ASSERT('$len == 0');
            break;
          case 8:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->nodesSiblingsBefore_[] = new NodeFull($fp, $len);
            ASSERT('$len == 0');
            break;
          case 9:
            ASSERT('$wire == 2');
            $len = \Protobuf::read_varint($fp, $limit);
            if ($len === false)
              throw new Exception('Protobuf::read_varint returned false');
            $limit-=$len;
            $this->nodesSiblingsAfter_[] = new NodeFull($fp, $len);
            ASSERT('$len == 0');
            break;
          default:
            $this->_unknown[$field . '-' . \Protobuf::get_wiretype($wire)][] = \Protobuf::read_field($fp, $wire, $limit);
        }
      }
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
    }

    function write($fp) {
      if (!$this->validateRequired())
        throw new Exception('Required fields are missing');
      if (!is_null($this->nodesAsc_))
        foreach($this->nodesAsc_ as $v) {
          fwrite($fp, "\x0a");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->edgesAsc_))
        foreach($this->edgesAsc_ as $v) {
          fwrite($fp, "\x12");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->familiesAsc_))
        foreach($this->familiesAsc_ as $v) {
          fwrite($fp, "\x1a");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->nodesDesc_))
        foreach($this->nodesDesc_ as $v) {
          fwrite($fp, "\"");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->edgesDesc_))
        foreach($this->edgesDesc_ as $v) {
          fwrite($fp, "*");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->familiesDesc_))
        foreach($this->familiesDesc_ as $v) {
          fwrite($fp, "2");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->nodesSiblings_))
        foreach($this->nodesSiblings_ as $v) {
          fwrite($fp, ":");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->nodesSiblingsBefore_))
        foreach($this->nodesSiblingsBefore_ as $v) {
          fwrite($fp, "B");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
      if (!is_null($this->nodesSiblingsAfter_))
        foreach($this->nodesSiblingsAfter_ as $v) {
          fwrite($fp, "J");
          \Protobuf::write_varint($fp, $v->size()); // message
          $v->write($fp);
        }
    }

    public function size() {
      $size = 0;
      if (!is_null($this->nodesAsc_))
        foreach($this->nodesAsc_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->edgesAsc_))
        foreach($this->edgesAsc_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->familiesAsc_))
        foreach($this->familiesAsc_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->nodesDesc_))
        foreach($this->nodesDesc_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->edgesDesc_))
        foreach($this->edgesDesc_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->familiesDesc_))
        foreach($this->familiesDesc_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->nodesSiblings_))
        foreach($this->nodesSiblings_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->nodesSiblingsBefore_))
        foreach($this->nodesSiblingsBefore_ as $v) {
          $l = $v->size();
          $size += 1 + \Protobuf::size_varint($l) + $l;
        }
      if (!is_null($this->nodesSiblingsAfter_))
        foreach($this->nodesSiblingsAfter_ as $v) {
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
           . \Protobuf::toString('nodesAsc_', $this->nodesAsc_)
           . \Protobuf::toString('edgesAsc_', $this->edgesAsc_)
           . \Protobuf::toString('familiesAsc_', $this->familiesAsc_)
           . \Protobuf::toString('nodesDesc_', $this->nodesDesc_)
           . \Protobuf::toString('edgesDesc_', $this->edgesDesc_)
           . \Protobuf::toString('familiesDesc_', $this->familiesDesc_)
           . \Protobuf::toString('nodesSiblings_', $this->nodesSiblings_)
           . \Protobuf::toString('nodesSiblingsBefore_', $this->nodesSiblingsBefore_)
           . \Protobuf::toString('nodesSiblingsAfter_', $this->nodesSiblingsAfter_);
    }

    // repeated .NodeFull nodes_asc = 1;

    private $nodesAsc_ = null;
    public function clearNodesAsc() { $this->nodesAsc_ = null; }
    public function getNodesAscCount() { if ($this->nodesAsc_ === null ) return 0; else return count($this->nodesAsc_); }
    public function getNodesAsc($index) { return $this->nodesAsc_[$index]; }
    public function getNodesAscArray() { if ($this->nodesAsc_ === null ) return array(); else return $this->nodesAsc_; }
    public function setNodesAsc($index, $value) {$this->nodesAsc_[$index] = $value;	}
    public function addNodesAsc($value) { $this->nodesAsc_[] = $value; }
    public function addAllNodesAsc(array $values) { foreach($values as $value) {$this->nodesAsc_[] = $value;} }

    // repeated .Edge edges_asc = 2;

    private $edgesAsc_ = null;
    public function clearEdgesAsc() { $this->edgesAsc_ = null; }
    public function getEdgesAscCount() { if ($this->edgesAsc_ === null ) return 0; else return count($this->edgesAsc_); }
    public function getEdgesAsc($index) { return $this->edgesAsc_[$index]; }
    public function getEdgesAscArray() { if ($this->edgesAsc_ === null ) return array(); else return $this->edgesAsc_; }
    public function setEdgesAsc($index, $value) {$this->edgesAsc_[$index] = $value;	}
    public function addEdgesAsc($value) { $this->edgesAsc_[] = $value; }
    public function addAllEdgesAsc(array $values) { foreach($values as $value) {$this->edgesAsc_[] = $value;} }

    // repeated .FamilyTreeFull families_asc = 3;

    private $familiesAsc_ = null;
    public function clearFamiliesAsc() { $this->familiesAsc_ = null; }
    public function getFamiliesAscCount() { if ($this->familiesAsc_ === null ) return 0; else return count($this->familiesAsc_); }
    public function getFamiliesAsc($index) { return $this->familiesAsc_[$index]; }
    public function getFamiliesAscArray() { if ($this->familiesAsc_ === null ) return array(); else return $this->familiesAsc_; }
    public function setFamiliesAsc($index, $value) {$this->familiesAsc_[$index] = $value;	}
    public function addFamiliesAsc($value) { $this->familiesAsc_[] = $value; }
    public function addAllFamiliesAsc(array $values) { foreach($values as $value) {$this->familiesAsc_[] = $value;} }

    // repeated .NodeFull nodes_desc = 4;

    private $nodesDesc_ = null;
    public function clearNodesDesc() { $this->nodesDesc_ = null; }
    public function getNodesDescCount() { if ($this->nodesDesc_ === null ) return 0; else return count($this->nodesDesc_); }
    public function getNodesDesc($index) { return $this->nodesDesc_[$index]; }
    public function getNodesDescArray() { if ($this->nodesDesc_ === null ) return array(); else return $this->nodesDesc_; }
    public function setNodesDesc($index, $value) {$this->nodesDesc_[$index] = $value;	}
    public function addNodesDesc($value) { $this->nodesDesc_[] = $value; }
    public function addAllNodesDesc(array $values) { foreach($values as $value) {$this->nodesDesc_[] = $value;} }

    // repeated .Edge edges_desc = 5;

    private $edgesDesc_ = null;
    public function clearEdgesDesc() { $this->edgesDesc_ = null; }
    public function getEdgesDescCount() { if ($this->edgesDesc_ === null ) return 0; else return count($this->edgesDesc_); }
    public function getEdgesDesc($index) { return $this->edgesDesc_[$index]; }
    public function getEdgesDescArray() { if ($this->edgesDesc_ === null ) return array(); else return $this->edgesDesc_; }
    public function setEdgesDesc($index, $value) {$this->edgesDesc_[$index] = $value;	}
    public function addEdgesDesc($value) { $this->edgesDesc_[] = $value; }
    public function addAllEdgesDesc(array $values) { foreach($values as $value) {$this->edgesDesc_[] = $value;} }

    // repeated .FamilyTreeFull families_desc = 6;

    private $familiesDesc_ = null;
    public function clearFamiliesDesc() { $this->familiesDesc_ = null; }
    public function getFamiliesDescCount() { if ($this->familiesDesc_ === null ) return 0; else return count($this->familiesDesc_); }
    public function getFamiliesDesc($index) { return $this->familiesDesc_[$index]; }
    public function getFamiliesDescArray() { if ($this->familiesDesc_ === null ) return array(); else return $this->familiesDesc_; }
    public function setFamiliesDesc($index, $value) {$this->familiesDesc_[$index] = $value;	}
    public function addFamiliesDesc($value) { $this->familiesDesc_[] = $value; }
    public function addAllFamiliesDesc(array $values) { foreach($values as $value) {$this->familiesDesc_[] = $value;} }

    // repeated .NodeFull nodes_siblings = 7;

    private $nodesSiblings_ = null;
    public function clearNodesSiblings() { $this->nodesSiblings_ = null; }
    public function getNodesSiblingsCount() { if ($this->nodesSiblings_ === null ) return 0; else return count($this->nodesSiblings_); }
    public function getNodesSiblings($index) { return $this->nodesSiblings_[$index]; }
    public function getNodesSiblingsArray() { if ($this->nodesSiblings_ === null ) return array(); else return $this->nodesSiblings_; }
    public function setNodesSiblings($index, $value) {$this->nodesSiblings_[$index] = $value;	}
    public function addNodesSiblings($value) { $this->nodesSiblings_[] = $value; }
    public function addAllNodesSiblings(array $values) { foreach($values as $value) {$this->nodesSiblings_[] = $value;} }

    // repeated .NodeFull nodes_siblings_before = 8;

    private $nodesSiblingsBefore_ = null;
    public function clearNodesSiblingsBefore() { $this->nodesSiblingsBefore_ = null; }
    public function getNodesSiblingsBeforeCount() { if ($this->nodesSiblingsBefore_ === null ) return 0; else return count($this->nodesSiblingsBefore_); }
    public function getNodesSiblingsBefore($index) { return $this->nodesSiblingsBefore_[$index]; }
    public function getNodesSiblingsBeforeArray() { if ($this->nodesSiblingsBefore_ === null ) return array(); else return $this->nodesSiblingsBefore_; }
    public function setNodesSiblingsBefore($index, $value) {$this->nodesSiblingsBefore_[$index] = $value;	}
    public function addNodesSiblingsBefore($value) { $this->nodesSiblingsBefore_[] = $value; }
    public function addAllNodesSiblingsBefore(array $values) { foreach($values as $value) {$this->nodesSiblingsBefore_[] = $value;} }

    // repeated .NodeFull nodes_siblings_after = 9;

    private $nodesSiblingsAfter_ = null;
    public function clearNodesSiblingsAfter() { $this->nodesSiblingsAfter_ = null; }
    public function getNodesSiblingsAfterCount() { if ($this->nodesSiblingsAfter_ === null ) return 0; else return count($this->nodesSiblingsAfter_); }
    public function getNodesSiblingsAfter($index) { return $this->nodesSiblingsAfter_[$index]; }
    public function getNodesSiblingsAfterArray() { if ($this->nodesSiblingsAfter_ === null ) return array(); else return $this->nodesSiblingsAfter_; }
    public function setNodesSiblingsAfter($index, $value) {$this->nodesSiblingsAfter_[$index] = $value;	}
    public function addNodesSiblingsAfter($value) { $this->nodesSiblingsAfter_[] = $value; }
    public function addAllNodesSiblingsAfter(array $values) { foreach($values as $value) {$this->nodesSiblingsAfter_[] = $value;} }

    // @@protoc_insertion_point(class_scope:GraphTreeFull)
  }

  // enum Sosa
  class Sosa {
    const SOSA_REF = 0;
    const SOSA = 1;
    const NO_SOSA = 2;

    public static $_values = array(
      0 => self::SOSA_REF,
      1 => self::SOSA,
      2 => self::NO_SOSA,
    );

    public static function toString($value) {
      if (is_null($value)) return null;
      if (array_key_exists($value, self::$_values))
        return self::$_values[$value];
      return 'UNKNOWN';
    }
  }

  // enum Calendar
  class Calendar {
    const GREGORIAN = 0;
    const JULIAN = 1;
    const FRENCH = 2;
    const HEBREW = 3;

    public static $_values = array(
      0 => self::GREGORIAN,
      1 => self::JULIAN,
      2 => self::FRENCH,
      3 => self::HEBREW,
    );

    public static function toString($value) {
      if (is_null($value)) return null;
      if (array_key_exists($value, self::$_values))
        return self::$_values[$value];
      return 'UNKNOWN';
    }
  }

  // enum Precision
  class Precision {
    const SURE = 0;
    const ABOUT = 1;
    const MAYBE = 2;
    const BEFORE = 3;
    const AFTER = 4;
    const ORYEAR = 5;
    const YEARINT = 6;

    public static $_values = array(
      0 => self::SURE,
      1 => self::ABOUT,
      2 => self::MAYBE,
      3 => self::BEFORE,
      4 => self::AFTER,
      5 => self::ORYEAR,
      6 => self::YEARINT,
    );

    public static function toString($value) {
      if (is_null($value)) return null;
      if (array_key_exists($value, self::$_values))
        return self::$_values[$value];
      return 'UNKNOWN';
    }
  }

  // enum Sex
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

  // enum DeathType
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

  // enum MarriageType
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

  // enum DivorceType
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

  // enum RelationType
  class RelationType {
    const RPARENT_ADOPTION = 0;
    const RPARENT_RECOGNITION = 1;
    const RPARENT_CANDIDATE_PARENT = 2;
    const RPARENT_GOD_PARENT = 3;
    const RPARENT_FOSTER_PARENT = 4;
    const RCHILD_ADOPTION = 5;
    const RCHILD_RECOGNITION = 6;
    const RCHILD_CANDIDATE_PARENT = 7;
    const RCHILD_GOD_PARENT = 8;
    const RCHILD_FOSTER_PARENT = 9;

    public static $_values = array(
      0 => self::RPARENT_ADOPTION,
      1 => self::RPARENT_RECOGNITION,
      2 => self::RPARENT_CANDIDATE_PARENT,
      3 => self::RPARENT_GOD_PARENT,
      4 => self::RPARENT_FOSTER_PARENT,
      5 => self::RCHILD_ADOPTION,
      6 => self::RCHILD_RECOGNITION,
      7 => self::RCHILD_CANDIDATE_PARENT,
      8 => self::RCHILD_GOD_PARENT,
      9 => self::RCHILD_FOSTER_PARENT,
    );

    public static function toString($value) {
      if (is_null($value)) return null;
      if (array_key_exists($value, self::$_values))
        return self::$_values[$value];
      return 'UNKNOWN';
    }
  }

  // enum WitnessType
  class WitnessType {
    const WITNESS = 0;
    const WITNESS_GODPARENT = 1;

    public static $_values = array(
      0 => self::WITNESS,
      1 => self::WITNESS_GODPARENT,
    );

    public static function toString($value) {
      if (is_null($value)) return null;
      if (array_key_exists($value, self::$_values))
        return self::$_values[$value];
      return 'UNKNOWN';
    }
  }

  // enum TitleType
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

}
