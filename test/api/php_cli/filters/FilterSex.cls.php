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

class FilterSex extends Filter {

	protected $sex;

	public function __construct($sex = null){
		$this->setSex($sex);
	}

	public function setSex($sex){
		$this->sex = $sex;
	}

	public function getSex(){
		return $this->sex;
	}

}
?>
