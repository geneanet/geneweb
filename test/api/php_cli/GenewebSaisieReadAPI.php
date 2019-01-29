<?php
/**
 * Classe de gestion de l'API de Geneweb
 * proto : api_saisie_read
 */

namespace Geneanet\Bundle\GenewebBundle\Api\SaisieRead;

require_once 'protocolbuffers.inc.php';
require_once 'api_saisie_read.proto.php';

use Geneanet\Bundle\GenewebBundle\Api\Object\Date as GenewebApiDate;
use Geneanet\Bundle\GenewebBundle\Api\SaisieRead\Object\GraphTreeParams;
use Geneanet\Bundle\GenewebBundle\Api\SaisieRead\Object\IndexPerson;

/**
 * GenewebSaisieReadAPI
 */
class GenewebSaisieReadAPI
{
    /** Sortie Protocol Buffers */
    const OUTPUT_PB = "pb";
    /** Sortie Binary */
    const OUTPUT_BINARY = "binary";
    /** Sortie JSON */
    const OUTPUT_JSON = "json";
    /** Sortie XML */
    const OUTPUT_XML = "xml";

    public $url = "http://localhost:2318/";

    protected $signature;

    public $base;

    protected $iz;

    protected $modeFriend = false;
    protected $modeWizard = false;

    protected $user = null;
    protected $password = null;

    protected $output = self::OUTPUT_PB;

    protected $fullInfos = false;

    protected $filters = array();

    protected static $instances = array();

        public    $lastUrl;

    /**
     * Constructeur public
     * @param string $base
     *
     * @return GenewebAPI
     */
    public function __construct($base)
    {
        $this->base = $base;
    }

    /**
     * Instance de classe
     * @param string $base
     * @param string $signature
     *
     * @return GenewebAPI
     *                    */
    public static function getInstance($base, $signature = "test")
    {
        if (!isset(self::$instances[$base])) {
            self::$instances[$base] =  new static($base);
        }

        self::$instances[$base]->signature = $signature;

        return self::$instances[$base];
    }

    /**
     * Active le mode wizard
     * @param string $user
     * @param string $password
     *
     * @return void
     *
     */
    public function enableModeWizard($user, $password)
    {
        $this->user = $user;
        $this->password = $password;
        $this->modeWizard = true;
        $this->modeFriend = false;
    }

    /**
     * Active le mode friend
     * @param string $user
     * @param string $password
     *
     * @return void
     *
     */
    public function enableModeFriend($user, $password)
    {
        $this->enableModeWizard($user, $password);
        $this->modeWizard = false;
        $this->modeFriend = true;
    }

    protected function isModeFriend()
    {
        return $this->modeFriend;
    }

    /**
     * [isModeWizard description]
     * @return boolean [description]
     */
    protected function isModeWizard()
    {
        return $this->modeWizard;
    }

    /**
     * Permet de définir le sosa via son index
     * @param int $iz
     *
     * @return void
     */
    public function setIz($iz)
    {
        $this->iz = $iz;
    }

    protected function getIz()
    {
        return $this->iz;
    }

    /**
     * Permet de définir le type de sortie
     * @param string $output (voir constante)
     *
     * @return void
     */
    public function setOutput($output)
    {
        $this->output = $output;
    }

    /**
     * Permet de définir le type d'infos souhaitées
     * (partielles ou complètes)
     * @param bool $fullInfos
     *
     * @return void
     */
    public function setFullInfos($fullInfos)
    {
        $this->fullInfos = (bool) $fullInfos;
    }

    protected function isFullInfos()
    {
        return (bool) $this->fullInfos;
    }

    /**
     * Ajoute un filtre
     * @param filters\Filter $filter
     *
     * @return void
     */
    public function addFilter(filters\Filter $filter)
    {
        $this->filters[] = $filter;
    }

    /**
     * Ré-initialise les filters
     *
     * @return void
     */
    public function resetFilters()
    {
        $this->filters = array();
    }


    /**
     * Get ascending and descending tree in a single object
     * @param  int $index     geneweb index of the root person
     * @param  int $nbGenAsc  number of ascinding generations requested
     * @param  int $nbGenDesc number of descending generations requested
     *
     * @return object\Graph   Tree
     */
    public function getTree($index, $nbGenAsc, $nbGenDesc)
    {
        $gap = new GraphTreeParams();
        $gap->setIndex($index);
        if ($nbGenAsc !== null && $nbGenAsc != "") {
            $gap->setNbAsc((int) $nbGenAsc);
        }
        if ($nbGenDesc !== null && $nbGenDesc != "") {
            $gap->setNbDesc((int) $nbGenDesc);
        }

        return $this->request("GRAPH_TREE", $gap, "\\Geneanet\\Bundle\\GenewebBundle\\Api\\SaisieRead\\Object\\GraphTree");
    }

    /**
     * Get ascending and descending tree in a single object (version 2)
     * @param  int $index     geneweb index of the root person
     * @param  int $nbGenAsc  number of ascinding generations requested
     * @param  int $nbGenDesc number of descending generations requested
     *
     * @return object\Graph   Tree
     */
    public function getTreeV2($index, $nbGenAsc, $nbGenDesc)
    {
        $gap = new GraphTreeParams();
        $gap->setIndex($index);
        if ($nbGenAsc !== null && $nbGenAsc != "") {
            $gap->setNbAsc((int) $nbGenAsc);
        }
        if ($nbGenDesc !== null && $nbGenDesc != "") {
            $gap->setNbDesc((int) $nbGenDesc);
        }

        return $this->request("GRAPH_TREE_V2", $gap, "\\Geneanet\\Bundle\\GenewebBundle\\Api\\SaisieRead\\Object\\GraphTree");

    }

    /**
     * Get ascending and descending tree in a single object (Full version)
     * @param  int $index     geneweb index of the root person
     * @param  int $nbGenAsc  number of ascinding generations requested
     * @param  int $nbGenDesc number of descending generations requested
     *
     * @return object\Graph   Tree
     */
    public function getTreeFull($index, $nbGenAsc, $nbGenDesc)
    {
        $gap = new GraphTreeParams();
        $gap->setIndex($index);
        if ($nbGenAsc !== null && $nbGenAsc != "") {
            $gap->setNbAsc((int) $nbGenAsc);
        }
        if ($nbGenDesc !== null && $nbGenDesc != "") {
            $gap->setNbDesc((int) $nbGenDesc);
        }

        return $this->request("GRAPH_TREE_FULL", $gap, "\\Geneanet\\Bundle\\GenewebBundle\\Api\\SaisieRead\\Object\\GraphTreeFull");
    }

    /**
     * Get full person informations
     * @param int $index geneweb index of the person
     *
     * @return PersonFull
     */
    public function getPerson($index)
    {
        $gap = new IndexPerson();
        $gap->setIndex($index);

        return $this->request("PERSON_TREE", $gap, "\\Geneanet\\Bundle\\GenewebBundle\\Api\\SaisieRead\\Object\\Person");
    }

    /**
     * Permet de récupérer un objet Date à partir d'une string date
     * @param string $s
     *
     * @return GenewebApiDate
     */
    public function getGenewebDateFromString($s)
    {
        return GenewebApiDate::fromGenewebDate($s);
    }

    /**
     * Envoi de la requête à l'API de Geneweb
     * @param string $type
     * @param mixed  $data
     * @param string $class
     *
     * @return mixed
     */
    protected function request($type, $data, $class)
    {
        $vars = array();
        $vars['m'] = 'API_'.$type;
        $vars['input'] = 'pb';
        $vars['output'] = ($this->output == self::OUTPUT_BINARY) ? self::OUTPUT_PB : $this->output;
        $vars['sig'] = $this->signature;
        if ($this->getIz() != "") {
            $vars['iz'] = $this->getIz();
        }
        $b = $this->base;
        // on vérifie que le mode magicien ou ami est activé
        $authorization = false;
        if ($this->isModeWizard()) {
            $b .= "_w";
            $authorization = true;
        } elseif ($this->isModeFriend()) {
            $b .= "_f";
            $authorization = true;
        }

        if ($data !== null) {
            if (get_class($data)) {
                $fp = fopen('php://memory', 'r+b');
                $data->write($fp);
                rewind($fp);
                $data = stream_get_contents($fp);
                fclose($fp);
            }
            $vars["data"] = $data; //urlencode($data);
        }

        if ($this->isFullInfos()) {
            $vars['full_infos'] = $this->isFullInfos();
        }
        $isNbResults = false;
        if (count($this->filters) > 0) {
            $filterAPI = new \Geneweb\Bundle\GenewebAPIBundle\Api\Object\Filters();
            foreach ($this->filters as $filter) {
                if ($filter instanceof filters\FilterOnlySosa) {
                    $filterAPI->setOnlySosa(true);
                } elseif ($filter instanceof filters\FilterOnlyRecent) {
                    $filterAPI->setOnlyRecent(true);
                } elseif ($filter instanceof filters\FilterSex) {
                    $filterAPI->setSex($filter->getSex());
                } elseif ($filter instanceof filters\FilterNbResults) {
                    $filterAPI->setNbResults(true);
                    $isNbResults = true;
                } elseif (($filter instanceof filters\FilterDateBirth)
                || ($filter instanceof filters\FilterDateDeath)) {
                    $filterDateB = new \Geneweb\Bundle\GenewebAPIBundle\Api\Object\FilterDate();
                    $filterDateB->setDay($filter->getDayBegin());
                    $filterDateB->setMonth($filter->getMonthBegin());
                    $filterDateB->setYear($filter->getYearBegin());
                    $filterDateE = new \Geneweb\Bundle\GenewebAPIBundle\Api\Object\FilterDate();
                    $filterDateE->setDay($filter->getDayEnd());
                    $filterDateE->setMonth($filter->getMonthEnd());
                    $filterDateE->setYear($filter->getYearEnd());
                    $filterRange = new \Geneweb\Bundle\GenewebAPIBundle\Api\Object\FilterDateRange();
                    $filterRange->setDateBegin($filterDateB);
                    $filterRange->setDateEnd($filterDateE);
                    if ($filter instanceof filters\FilterDateBirth) {
                        $filterAPI->setDateBirth($filterRange);
                    } else {
                        $filterAPI->setDateDeath($filterRange);
                    }
                }
            }

            $fp = fopen('php://memory', 'r+b');
            $filterAPI->write($fp);
            rewind($fp);
            $data = stream_get_contents($fp);
            fclose($fp);
            $vars["filters"] = $data; //urlencode($data);
        }
        $this->filters = array();

        $query = http_build_query($vars); //for GET
        $req = $this->url.$b."?".$query;

        $this->lastUrl = $req;

        $ci = curl_init();
        curl_setopt($ci, CURLOPT_URL, $req);
        curl_setopt($ci, CURLOPT_HEADER, false);
        curl_setopt($ci, CURLOPT_RETURNTRANSFER, true);
        // headers
        $headers = array();

        //si le mode ami ou magicien est activé, on envoie les authentifications
        if ($authorization) {
            array_push($headers, 'Authorization: Basic '.base64_encode($this->user.":".$this->password));
        }

        //liens inter-arbres
        //if ($this->linksTree) {
            array_push($headers, 'Links-Tree: 1');
        //}

        //on ajoute les headers
        if (!empty($headers)) {
           curl_setopt($ci, CURLOPT_HTTPHEADER, $headers);
        }
        $data = null;

        $dataEncoded = curl_exec($ci);
        if ($isNbResults) {
            $class = $this->getClassname("InternalInt32");
        }

        if ($class !== null) {
            if (curl_errno($ci)) {
                $data = new $class();
            } else {
                try {
                    if ($this->output === self::OUTPUT_PB) {
                        $data = new $class($dataEncoded);
                        if ($isNbResults) {
                            $data = $data->getValue();
                        }
                    } else {
                        $data = $dataEncoded;
                    }
                } catch (\Exception $e) {
                    if ($this->output === self::OUTPUT_PB) {
                        $data = new $class();
                        if ($isNbResults) {
                            $data = $data->getValue();
                        }
                    } else {
                        $data = $dataEncoded;
                    }
                }
                curl_close($ci);
            }
        }

        return $data;
    }

    /**
     * Set the value of GenewebSaisieReadAPI
     *
     * @param mixed url
     *
     * @return self
     */
    public function setUrl($url)
    {
        $this->url = $url;

        return $this;
    }

    /**
     * Set the value of Signature
     *
     * @param mixed signature
     *
     * @return self
     */
    public function setSignature($signature)
    {
        $this->signature = $signature;

        return $this;
    }

    /**
     * Set the value of Base
     *
     * @param mixed base
     *
     * @return self
     */
    public function setBase($base)
    {
        $this->base = $base;

        return $this;
    }

    /**
     * Set the value of Mode Friend
     *
     * @param mixed modeFriend
     *
     * @return self
     */
    public function setModeFriend($modeFriend)
    {
        $this->modeFriend = $modeFriend;

        return $this;
    }

    /**
     * Set the value of Mode Wizard
     *
     * @param mixed modeWizard
     *
     * @return self
     */
    public function setModeWizard($modeWizard)
    {
        $this->modeWizard = $modeWizard;

        return $this;
    }

    /**
     * Set the value of User
     *
     * @param mixed user
     *
     * @return self
     */
    public function setUser($user)
    {
        $this->user = $user;

        return $this;
    }

    /**
     * Set the value of Password
     *
     * @param mixed password
     *
     * @return self
     */
    public function setPassword($password)
    {
        $this->password = $password;

        return $this;
    }

    /**
     * Set the value of Filters
     *
     * @param mixed filters
     *
     * @return self
     */
    public function setFilters($filters)
    {
        $this->filters = $filters;

        return $this;
    }

    /**
     * Set the value of Instances
     *
     * @param mixed instances
     *
     * @return self
     */
    public function setInstances($instances)
    {
        $this->instances = $instances;

        return $this;
    }


    /**
     * Get the value of GenewebSaisieReadAPI
     *
     * @return mixed
     */
    public function getUrl()
    {
        return $this->url;
    }

    /**
     * Get the value of Signature
     *
     * @return mixed
     */
    public function getSignature()
    {
        return $this->signature;
    }

    /**
     * Get the value of Base
     *
     * @return mixed
     */
    public function getBase()
    {
        return $this->base;
    }

    /**
     * Get the value of Mode Friend
     *
     * @return mixed
     */
    public function getModeFriend()
    {
        return $this->modeFriend;
    }

    /**
     * Get the value of Mode Wizard
     *
     * @return mixed
     */
    public function getModeWizard()
    {
        return $this->modeWizard;
    }

    /**
     * Get the value of User
     *
     * @return mixed
     */
    public function getUser()
    {
        return $this->user;
    }

    /**
     * Get the value of Password
     *
     * @return mixed
     */
    public function getPassword()
    {
        return $this->password;
    }

    /**
     * Get the value of Output
     *
     * @return mixed
     */
    public function getOutput()
    {
        return $this->output;
    }

    /**
     * Get the value of Full Infos
     *
     * @return mixed
     */
    public function getFullInfos()
    {
        return $this->fullInfos;
    }

    /**
     * Get the value of Filters
     *
     * @return mixed
     */
    public function getFilters()
    {
        return $this->filters;
    }

    /**
     * Get the value of Instances
     *
     * @return mixed
     */
    public function getInstances()
    {
        return $this->instances;
    }

}
