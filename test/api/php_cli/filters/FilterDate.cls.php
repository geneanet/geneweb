<?php
/**
 *
 * Filter
 *
 * @author Harrich SANDIRASSEGARANE
 * @version 0.1
 *
 * Rev 0.1 du 2012/09/27
 * - version initiale
 */

namespace geneweb\api\filters;

abstract class FilterDate extends Filter {

	protected $dayBegin = 0;
	protected $monthBegin = 0;
	protected $yearBegin = 0;
	protected $dayEnd = 0;
	protected $monthEnd = 0;
	protected $yearEnd = 0;

	public function __construct(){
	}

	public function setDayBegin($dayBegin){
		$this->dayBegin = (int)$dayBegin;
	}

	public function getDayBegin(){
		return $this->dayBegin;
	}

	public function setMonthBegin($monthBegin){
		$this->monthBegin = (int)$monthBegin;
	}

	public function getMonthBegin(){
		return $this->monthBegin;
	}

	public function setYearBegin($yearBegin){
		$this->yearBegin = (int)$yearBegin;
	}

	public function getYearBegin(){
		return $this->yearBegin;
	}

	public function setDayEnd($dayEnd){
		$this->dayEnd = (int)$dayEnd;
	}

	public function getDayEnd(){
		return $this->dayEnd;
	}

	public function setMonthEnd($monthEnd){
		$this->monthEnd = (int)$monthEnd;
	}

	public function getMonthEnd(){
		return $this->monthEnd;
	}

	public function setYearEnd($yearEnd){
		$this->yearEnd = (int)$yearEnd;
	}

	public function getYearEnd(){
		return $this->yearEnd;
	}

}
?>
