<?php
/**
 * Classe de gestion de l'API de Geneweb
 * GenewebAPI
 * GenewebAPI.cls.php
 *
 * @author : harrich
 * @version: 0.21
 *
 * Rev 0.21 du 2013/04/30 (Harrich)
 * Fix bug #1524: Gestion de l'oc négatif
 * Rev 0.2 du 2012/03/15 (Harrich, Julien)
 * - Ajout de getLoopBase()
 * Rev 0.1 du 2011/11/23
 * - Version initiale
 *
 */
namespace geneweb\api;
require_once('protocolbuffers.inc.php');
require_once('api.proto.php');
use geneweb\api\object\ListPersons;
use geneweb\api\object\Person;
use geneweb\api\object\InfosBase;

class GenewebAPI {

    /** Sortie Protocol Buffers */
    const OUTPUT_PB = "pb";
    /** Sortie Binary */
    const OUTPUT_BINARY = "binary";
    /** Sortie JSON */
    const OUTPUT_JSON = "json";
    /** Sortie XML */
    const OUTPUT_XML = "xml";

    protected $url = "http://localhost:2318/";

    private $signature;

    protected $base;

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
     * Constructeur privé
     * @return GenewebAPI
     */
    public function __construct($base){
        $this->base = $base;
    }

    /**
     * Instance de classe
     * @param string $base
     * @param string $signature
     * @return GenewebAPI
     * */
    public static function getInstance($base, $signature) {
        if(!isset(self::$instances[$base])){
            self::$instances[$base] =  new static($base);
        }

        self::$instances[$base]->signature = $signature;
        return self::$instances[$base];
    }

    public function enableModeWizard($user, $password){
        $this->user = $user;
        $this->password = $password;
        $this->modeWizard = true;
        $this->modeFriend = false;
    }

    public function enableModeFriend($user, $password){
        $this->enableModeWizard($user, $password);
        $this->modeWizard = false;
        $this->modeFriend = true;
    }

    protected function isModeFriend(){
        return $this->modeFriend;
    }

    protected function isModeWizard(){
        return $this->modeWizard;
    }

    public function setIz($iz){
        $this->iz = $iz;
    }

    protected function getIz(){
        return $this->iz;
    }

    /**
     * Permet de définir le type de sortie
     * @param string $output (voir constante)
     * @return void
     */
    public function setOutput($output){
        $this->output = $output;
    }

    /**
     * Permet de définir le type d'infos souhaitées
     * (partielles ou complètes)
     * @param bool $fullInfos
     * @return void
     */
    public function setFullInfos($fullInfos){
        $this->fullInfos = (bool)$fullInfos;
    }

    protected function isFullInfos(){
        return (bool)$this->fullInfos;
    }

    public function addFilter(filters\Filter $filter){
        $this->filters[] = $filter;
    }
    public function resetFilter(){
        $this->filters = array();
    }
    private function getClassname($classname){
        if($this->isFullInfos()){
            switch($classname){
            case 'Person':
                $classname = 'FullPerson';
                break;
            case 'ListPersons':
                $classname = 'ListFullPersons';
                break;
            case 'Family':
                $classname = 'FullFamily';
                break;
            case 'Image':
                $classname = 'FullImage';
                break;
            case 'ListImages':
                $classname = 'ListFullImages';
                break;
            case 'PersonRelation':
                $classname = 'FullPersonRelation';
                break;
            case 'ListPersonRelation':
                $classname = 'ListFullPersonRelation';
                break;
            case 'Node':
                $classname = 'FullNode';
                break;
            case 'Graph':
                $classname = 'FullGraph';
                break;
            }
        }
        return "\geneweb\api\object\\".$classname;
    }

    /**
         * Permet de rechercher les individus commençant par le nom ET le prénom
         * Exemple: $lastname = mar et $firstname = jean renverra
         * MARTIN Jean / MARTIN Jean Nicolas / MARTINIERE Jeanne
         * @param string $lastname
         * @param string $firstname
         * @param bool $onlySosa
         * @param bool $onlyRecent
         * @return object\ListPersons
         */
    public function startingWithSearch($lastname, $firstname = null, $onlySosa = false, $onlyRecent = false){
        $sp = new \geneweb\api\object\SearchParams();
        $sp->setSearchType(\geneweb\api\object\SearchType::STARTING_WITH);
        if($lastname !== ""){
            $sp->setLastname($lastname);
        }
        if($firstname !== null && $firstname !== ""){
            $sp->setFirstname($firstname);
        }
        if((bool)$only_sosa){
            $this->addFilter(new filters\FilterOnlySosa());
        }
        if((bool)$only_recent){
            $this->addFilter(new filters\FilterOnlyRecent());
        }
        return $this->request("SEARCH", $sp, $this->getClassname('ListPersons'));
    }


    /**
         * Permet de rechercher les individus contenant le nom ET le prénom
         * Exemple: $lastname = mar et $firstname = jean renverra
         * MARTIN Jean / MARTIN Nicolas Jean / GALLIMARD Claudette-Jeanne
         * @param string $lastname
         * @param string $firstname
         * @param bool $only_sosa
         * @param bool $only_recent
         * @param bool $maiden_name
         * @return object\ListPersons
         */
    public function approximativeSearch($lastname, $firstname = null, $only_sosa = false, $only_recent = false, $maiden_name = false){
        $sp = new \geneweb\api\object\SearchParams();
        $sp->setSearchType(\geneweb\api\object\SearchType::APPROXIMATIVE);
        $sp->setLastname($lastname);
        if($firstname !== null && $firstname !== ""){
            $sp->setFirstname($firstname);
        }
        if((bool)$only_sosa){
            $this->addFilter(new filters\FilterOnlySosa());
        }
        if((bool)$only_recent){
            $this->addFilter(new filters\FilterOnlyRecent());
        }
//        $sp->setOnlySosa((bool)$only_sosa);
//        $sp->setOnlyRecent((bool)$only_recent);
        $sp->setMaidenName((bool)$maiden_name);
        return $this->request("SEARCH", $sp, $this->getClassname('ListPersons'));
    }

    /**
     * Permet de rechercher les individus contenant le nom OU le prénom
     * Exemple: $lastname = mar et $firstname = jean renverra
     * DUPOND Jean  / MARTIN Pierre / MARTIN Nicolas Jean / ...
     * @param string $lastname
     * @param string $firstname
     * @return object\ListPersons
     */
    public function lastnameOrFirstnameSearch($lastname, $firstname){
        $sp = new \geneweb\api\object\SearchParams();
        $sp->setSearchType(\geneweb\api\object\SearchType::LASTNAME_OR_FIRSTNAME);
        $sp->setLastname($lastname);
        $sp->setFirstname($firstname);
        if((bool)$only_sosa){
            $this->addFilter(new filters\FilterOnlySosa());
        }
        if((bool)$only_recent){
            $this->addFilter(new filters\FilterOnlyRecent());
        }
        //$sp->setOnlySosa((bool)$only_sosa);
        //$sp->setOnlyRecent((bool)$only_recent);
        return $this->request("SEARCH", $sp, $this->getClassname('ListPersons'));
    }

    /**
     * Permet de récupérer les infos d'un individu
     * @param string $n
     * @param string $p
     * @param string $oc
     * @return object\Person
     *
     */
    public function getIndividualInfos($n, $p, $oc){
        //valeur négative impossible
        if((int)$oc < 0){
            $classname = $this->getClassname('Person');
            $obj = new $classname;
            $obj->setN((string)$n);
            $obj->setP((string)$p);
            $obj->setOc((int)$oc);
            if($this->output == self::OUTPUT_PB){
                return $obj;
            }
            return "";
        }
        else{
            $rp = new \geneweb\api\object\ReferencePerson();
            $rp->setN((string)$n);
            $rp->setP((string)$p);
            $rp->setOc((int)$oc);
            return $this->request("INFO_IND", $rp, $this->getClassname('Person'));
        }
    }

    /**
     * Permet de récupérer les infos d'une liste d'individus
     * Exemple: $array = array(array('n' => 'n', 'p' => 'p', 'oc' => 'oc'));
     * @param array $array
     * @return object\ListPerson
     */
    public function getListIndividualInfos($array){
        $list = new \geneweb\api\object\ListReferencePersons();
        foreach($array AS $a){
            $rp = new \geneweb\api\object\ReferencePerson();
            $rp->setN((string)$a['n']);
            $rp->setP((string)$a['p']);
            $rp->setOc((int)$a['oc']);
            if((int)$a['oc'] >= 0){
                $list->addListRefPersons($rp);
            }
        }
        return $this->request("LIST_PERSONS", $list, $this->getClassname('ListPersons'));
    }

    /**
     * Permet de récupérer toutes les personnes d'une base
     * @param int $from [optional]
     * @param int $limit [optional]
     * @return object\ListPerson
     */
    public function getAllPersons($from = null, $limit = null){
        $app = new \geneweb\api\object\AllPersonsParams();
        if($from != null){
            $app->setFrom((int)$from);
        }
        if($limit != null){
            $app->setLimit((int)$limit);
        }
        return $this->request("ALL_PERSONS", $app, $this->getClassname('ListPersons'));
    }

    /**
     * Permet de récupérer toutes les personnes d'une base
     * @param int $from [optional]
     * @param int $limit [optional]
     * @return object\ListPerson
     */
    public function prepareAllPersons($from = null, $limit = null, $protoFilter = null){
        $app = new \geneweb\api\object\AllPersonsParams();
        if($from != null){
            $app->setFrom((int)$from);
        }
        if($limit != null){
            $app->setLimit((int)$limit);
        }

        return $this->prepareRequest("ALL_PERSONS", $app, $this->getClassname('ListPersons'), $protoFilter);
    }

    /**
     * Permet de récupérer toutes les familles d'une base
     * @param int $from [optional]
     * @param int $limit [optional]
     * @return object\ListPerson
     */
    public function getAllFamilies($from = null, $limit = null){
        $afp = new \geneweb\api\object\AllFamiliesParams();
        if($from != null){
            $afp->setFrom((int)$from);
        }
        if($limit != null){
            $afp->setLimit((int)$limit);
        }
        return $this->request("ALL_FAMILIES", $afp, $this->getClassname('ListFullFamilies'));
    }

    /**
     * Permet d'ajouter une personne
     * @param $person object\Person
     * @return object\ModificationStatus
     */
    public function addPerson($person){
        return $this->request("ADD_PERSON_OK", $person, $this->getClassname('ModificationStatus'));
    }

    /**
     * Permet d'ajouter une personne
     * @param $person object\Person
     * @return object\ModificationStatus
     */
    public function addPersonStartOk($person){
        return $this->request("ADD_PERSON_START_OK", $person, $this->getClassname('ReferencePerson'));
    }

    /**
     * Permet de mettre à jour une personne
     * @param $person object\FullPerson
     * @return object\ModificationStatus
     */
    public function updatePerson($person){
        return $this->request("UPDATE_PERSON", $person, $this->getClassname('ModificationStatus'));
    }

    /**
     * Permet de supprimer une personne
     * @param $person object\FullPerson
     * @return object\ModificationStatus
     */
    public function deletePerson($person){
        return $this->request("DELETE_PERSON", $person, $this->getClassname('ModificationStatus'));
    }

    /**
     * Permet d'ajouter une famille
     * @param $person object\FullFamily
     * @return object\ModificationStatus
     */
    public function addFamily($family){
        return $this->request("ADD_FAMILY", $family, $this->getClassname('ModificationStatus'));
    }

    /**
     * Permet de mettre à jour une personne
     * @param $person object\FullPerson
     * @return object\ModificationStatus
     */
    public function updateFamily($family){
        return $this->request("UPDATE_FAMILY", $family, $this->getClassname('ModificationStatus'));
    }

    /**
     * Permet de supprimer une personne
     * @param $person object\FullPerson
     * @return object\ModificationStatus
     */
    public function deleteFamily($family){
        return $this->request("DELETE_FAMILY", $family, $this->getClassname('ModificationStatus'));
    }

    public function printPersons(){
        return $this->request("PRINT_INDEX", null, null);
    }
    /**
         * Permet de mettre à jour l'image d'une liste de personnes
         * Exemple: $array = array(array('n' => 'n', 'p' => 'p', 'oc' => 'oc', 'img' => 'http://...'));
         * @param array $array
         * @return void
         */
        public function updateListImagePerson($array){
                $list = new \geneweb\api\object\ListPersImg();
                foreach($array AS $a){
                        $pi = new \geneweb\api\object\PersImg();
            $rp = new \geneweb\api\object\ReferencePerson();
                        $rp->setN((string)$a['n']);
                        $rp->setP((string)$a['p']);
                        $rp->setOc((int)$a['oc']);
            $pi->setPerson($rp);
            $pi->setImg((string)$a['img']);
                        $list->addListPersImg($pi);
                }
                return $this->request("IMAGE_UPDATE", $list, null);
        }

    /**
         * Permet de mettre à jour l'image d'une personne
         * @param string $n
     * @param string $p
     * @param int $oc
      * @param string $url
         * @return void
         */
        public function updateImagePerson($n, $p, $oc, $url){
                return $this->updateListImagePerson(array(array('n' => (string)$n, 'p' => (string)$p, 'oc' => (int)$oc, 'img' => (string)$url)));
    }

    /**
     * Permet de récupérer une liste de personnes proches d'une personne
     * @param string $n
     * @param string $p
     * @param string $oc
     * @param bool $onlyRecent
     * @param bool $spouseAscend
     * @param int $nbGenAsc
     * @param int $nbGenDesc
     * @param bool $spouseAscend
     * @return  object\ListPerson
     */
    public function getClosePersons($n, $p, $oc, $onlyRecent = null, $spouseAscend = null, $nbGenAsc = null, $nbGenDesc = null){
        $ccp = new \geneweb\api\object\ClosePersonsParams();
        $rp = new \geneweb\api\object\ReferencePerson();
        $rp->setN((string)$n);
        $rp->setP((string)$p);
        $rp->setOc((int)$oc);
        $ccp->setPerson($rp);
        if($nbGenAsc !== null && $nbGenAsc != ""){
            $ccp->setNbGenAsc((int)$nbGenAsc);
        }
        if($nbGenDesc !== null && $nbGenDesc != ""){
            $ccp->setNbGenDesc((int)$nbGenDesc);
        }
        if($spouseAscend !== null && $spouseAscend !== ""){
            $ccp->setSpouseAscend((bool)$spouseAscend);
        }
        if($onlyRecent !== null && $onlyRecent !== ""){
            $ccp->setOnlyRecent((bool)$onlyRecent);
        }
        return $this->request("CLOSE_PERSONS", $ccp, $this->getClassname('ListPersonRelation'));
    }

    /**
     * Permet de récupérer une liste de personnes ayant leur anniversaire dans la fourchette de date donnée
     * 1 => janvier, 12 => décembre
     * @param int $month1
     * @param int $day1
     * @param int $month2
     * @param int $day2
         * @return  object\ListPerson
         */
    public function getBirthdayPersons($month1, $day1 = null, $month2 = null, $day2 = null){
        if($month2 == null) $month2 = $month1;
        if($day2 == null) $day2 = $day1;
        $filter = new filters\FilterDateBirth();
        $filter->setDayBegin($day1);
        $filter->setMonthBegin($month1);
        $filter->setDayEnd($day2);
        $filter->setMonthEnd($month2);
        $this->addFilter($filter);
        $this->addFilter(new filters\FilterOnlyRecent());
        return $this->getAllPersons();
    }

    /**
     * Permet de récupérer une liste de personnes ayant leur anniversaire dans la date donnée
     * @param string $n
     * @param string $p
     * @param int $oc
     * @param string $type
     * @param int $month
     * @param int $day
     * @return object\NotificationBirthday
     */
    public function getNotificationBirthday($n, $p, $oc, $type, $month, $day){
        $nbp = new \geneweb\api\object\NotificationBirthdayParams();
        $rp = new \geneweb\api\object\ReferencePerson();
        $rp->setN((string)$n);
        $rp->setP((string)$p);
        $rp->setOc((int)$oc);
        $nbp->setPerson($rp);
        $nbp->setMonth((int)$month);
        $nbp->setDay((int)$day);
        $nbp->setParams($type);
        return $this->request("NOTIFICATION_BIRTHDAY", $nbp, $this->getClassname('NotificationBirthday'));
    }

    /**
     * Permet de récupérer des infos sur la base
     * @return object\BaseInfos
     */
    public function getBaseInfos(){
        return $this->request("INFO_BASE", null, $this->getClassname('InfosBase'));
    }

    /**
     * Permet de récupérer tous les warnings d'une base
     * @return object\BaseWarnings
     */
    public function getBaseWarnings(){
        return $this->request("BASE_WARNINGS", null, $this->getClassname('BaseWarnings'));
    }

    /**
         * Permet de récupérer le graphe d'ascendance d'une personne
         * @param string $n
         * @param string $p
         * @param string $oc
         * @param int $generation
         * @return  object\Graph
         */
        public function getGraphAscPerson($n, $p, $oc, $generation){
                $gap = new \geneweb\api\object\GraphParams();
                $rp = new \geneweb\api\object\ReferencePerson();
                $rp->setN((string)$n);
                $rp->setP((string)$p);
                $rp->setOc((int)$oc);
                $gap->setPerson($rp);
                if($generation !== null && $generation != ""){
                        $gap->setGeneration((int)$generation);
                }
                return $this->request("GRAPH_ASC_LIA", $gap, $this->getClassname('Graph'));
        }

    /**
         * Permet de récupérer le graphe de descendance d'une personne
         * @param string $n
         * @param string $p
         * @param string $oc
         * @param int $generation
         * @return  object\Graph
         */
        public function getGraphDescPerson($n, $p, $oc, $generation){
                $gap = new \geneweb\api\object\GraphParams();
                $rp = new \geneweb\api\object\ReferencePerson();
                $rp->setN((string)$n);
                $rp->setP((string)$p);
                $rp->setOc((int)$oc);
                $gap->setPerson($rp);
                if($generation !== null && $generation != ""){
                        $gap->setGeneration((int)$generation);
                }
                return $this->request("GRAPH_DESC", $gap, $this->getClassname('Graph'));
        }

    /**
         * Permet de récupérer le graphe entre deux relations
         * @param string $n1
         * @param string $p1
     * @param string $oc1
     * @param string $n2
     * @param string $p2
     * @param string $oc2
         * @return  object\Graph
         */
        public function getGraphRelation($n1, $p1, $oc1, $n2, $p2, $oc2){
                $grp = new \geneweb\api\object\GraphRelParams();
                $rp1 = new \geneweb\api\object\ReferencePerson();
                $rp1->setN((string)$n1);
                $rp1->setP((string)$p1);
                $rp1->setOc((int)$oc1);
        $grp->setPerson1($rp1);
        $rp2 = new \geneweb\api\object\ReferencePerson();
        $rp2->setN((string)$n2);
        $rp2->setP((string)$p2);
        $rp2->setOc((int)$oc2);
        $grp->setPerson2($rp2);
                return $this->request("GRAPH_REL", $grp, $this->getClassname('Graph'));
        }

    /**
         * Permet de récupérer le couple commun entre deux individus
         * @param string $n1
         * @param string $p1
         * @param string $oc1
         * @param string $n2
         * @param string $p2
         * @param string $oc2
         * @return  object\ListPersons
         */
        public function getCplRelation($n1, $p1, $oc1, $n2, $p2, $oc2){
                $crp = new \geneweb\api\object\CplRelParams();
                $rp1 = new \geneweb\api\object\ReferencePerson();
                $rp1->setN((string)$n1);
                $rp1->setP((string)$p1);
                $rp1->setOc((int)$oc1);
                $crp->setPerson1($rp1);
                $rp2 = new \geneweb\api\object\ReferencePerson();
                $rp2->setN((string)$n2);
                $rp2->setP((string)$p2);
                $rp2->setOc((int)$oc2);
                $crp->setPerson2($rp2);
                return $this->request("CPL_REL", $crp, $this->getClassname('ListPersons'));
        }

    /**
     * Permet de récupérer la boucle de la base
     * @return object\Person
     */
    public function getLoopBase(){
        return $this->request("LOOP_BASE", null, $this->getClassname('Person'));
    }

    /**
         * Permet de récupérer toutes les images
         * @return object\ListImages
         */
        public function getAllImagesInt(){
                return $this->request("IMAGE", null, $this->getClassname('ListImages'));
    }

    /**
         * Permet de récupérer toutes les images
         * @return object\ListImages
         */
        public function getAllImagesIntNb(){
        $this->addFilter(new filters\FilterNbResults());
                return $this->request("IMAGE", null, $this->getClassname('ListImages'));
        }

    /**
         * Permet de récupérer toutes les images
         * @return object\ListImages
         */
        public function getAllImages(){
                return $this->request("IMAGE_ALL", null, $this->getClassname('ListImages'));
    }

    /**
         * Permet de récupérer toutes les images
         * @return object\ListImages
         */
        public function getAllImagesNb(){
        $this->addFilter(new filters\FilterNbResults());
                return $this->request("IMAGE_ALL", null, $this->getClassname('ListImages'));
        }

    /**
         * Permet de récupérer toutes les images internes
     * @deprecated DO NOT USE THIS !
         * @return object\ListImages
         */
        public function getAllImagesExt(){
                return $this->request("IMAGE_EXT", null, $this->getClassname('ListImages'));
        }

    /**
         * Permet de récupérer toutes les images externes
         * @deprecated DO NOT USE THIS !
         * @return object\ListImages
         */
        public function getAllImagesExtNb(){
        $this->addFilter(new filters\FilterNbResults());
        return $this->request("IMAGE_EXT", null, $this->getClassname('ListImages'));
    }

    /**
         * Permet de récupérer toutes les images externes (http uniquement)
     * @deprecated DO NOT USE THIS !
         * @return object\ListImages
         */
        public function removeImagesExt(){
                return $this->request("REMOVE_IMAGE_EXT", null, null);
        }

    /**
         * Permet de récupérer toutes les images externes (toutes)
     * @deprecated DO NOT USE THIS !
         * @return object\ListImages
         */
        public function removeAllImagesExt(){
                return $this->request("REMOVE_IMAGE_EXT_ALL", null, null);
        }

    /**
     * Permet de récupérer la référence personne à partir de son index
     * @param int id
     * @return object\ImageAddress
     */
    public function getReferencePerson($id){
        $index = new \geneweb\api\object\Index();
        $index->setIndex($id);
        return $this->request("REF_PERSON_FROM_ID", $index, $this->getClassname('ReferencePerson'));
    }


    /**
     * Permet de récupérer l'image d'une personne à partir de son index
     * @param int id
     * @return object\ImageAddress
     */
    public function getImagePerson($id){
        $index = new \geneweb\api\object\Index();
        $index->setIndex($id);
        return $this->request("IMAGE_PERSON", $index, $this->getClassname('ImageAddress'));
    }

    /**
     * Permet de récupérer le sosa à partir d'une personne
     * @param string $n
     * @param string $p
     * @param string $oc
     * @return object\ReferencePerson
     */
    public function findSosa($n, $p, $oc){
        $rp = new \geneweb\api\object\ReferencePerson();
        $rp->setN((string)$n);
        $rp->setP((string)$p);
        $rp->setOc((int)$oc);
        return $this->request("FIND_SOSA", $rp, $this->getClassname('ReferencePerson'));
    }

    /**
     * Permet de récupérer la personne ayant le plus d'ancêtre
     * @return object\ReferencePerson
     */
    public function getMaxAncestors(){
        return $this->request("MAX_ANCESTORS", null, $this->getClassname('ReferencePerson'));
    }


    public function exportBase($directory){
        $this->output = self::OUTPUT_BINARY;
        return $this->request("PRINT_EXPORT", $directory, NULL);
    }

    public function exportBaseSearch($directory){
        $this->output = self::OUTPUT_BINARY;
        return $this->request("PRINT_EXPORT_SEARCH", $directory, NULL);
    }

    public function synchroBase($directory,$timestamp){
        $params = new \geneweb\api\object\SynchroParams();
        $params->setExportDirectory($directory);
        $params->setTimestamp($timestamp);
        $this->output = self::OUTPUT_BINARY;
        return $this->request("PRINT_SYNCHRO", $params, NULL);
    }

    ########################################################
    #               REQUEST                                #
    ########################################################


    /**
     * Envoi de la requête à l'API de Geneweb
     * @param string $type
     * @param mixed $object
     * @param string $class
     * @return mixed
     */
    protected function request($type, $data, $class){

        $vars = array();
        $vars['m'] = 'API_'.$type;
        $vars['input'] = 'pb';
        $vars['output'] = ($this->output == self::OUTPUT_BINARY) ? self::OUTPUT_PB : $this->output;
        $vars['sig'] = $this->signature;
        if($this->getIz() != ""){
            $vars['iz'] = $this->getIz();
        }
        $b = $this->base;
        // on vérifie que le mode magicien ou ami est activé
        $authorization = false;
        if($this->isModeWizard()){
                $b .= "_w";
                $authorization = true;
        }
        elseif($this->isModeFriend()){
                $b .= "_f";
                $authorization = true;
        }

        if($data !== null){
            if(get_class($data)){
                $fp = fopen('php://memory', 'r+b');
                $data->write($fp);
                rewind($fp);
                $data = stream_get_contents($fp);
                fclose($fp);
            }
            $vars["data"] = $data; //urlencode($data);
        }

        if($this->isFullInfos()){
            $vars['full_infos'] = $this->isFullInfos();
        }
        $isNbResults = false;
        if(count($this->filters) > 0){
            $filterAPI = new \geneweb\api\object\Filters();
            foreach($this->filters AS $filter){
                if($filter instanceof filters\FilterOnlySosa){
                    $filterAPI->setOnlySosa(true);
                }
                elseif($filter instanceof filters\FilterOnlyRecent){
                    $filterAPI->setOnlyRecent(true);
                }
                elseif($filter instanceof filters\FilterSex){
                    $filterAPI->setSex($filter->getSex());
                }
                elseif($filter instanceof filters\FilterNbResults){
                    $filterAPI->setNbResults(true);
                    $isNbResults = true;
                }
                elseif(($filter instanceof filters\FilterDateBirth)
                           || ($filter instanceof filters\FilterDateDeath)){
                    $filterDateB = new \geneweb\api\object\FilterDate();
                    $filterDateB->setDay($filter->getDayBegin());
                    $filterDateB->setMonth($filter->getMonthBegin());
                    $filterDateB->setYear($filter->getYearBegin());
                    $filterDateE = new \geneweb\api\object\FilterDate();
                    $filterDateE->setDay($filter->getDayEnd());
                    $filterDateE->setMonth($filter->getMonthEnd());
                    $filterDateE->setYear($filter->getYearEnd());
                    $filterRange = new \geneweb\api\object\FilterDateRange();
                    $filterRange->setDateBegin($filterDateB);
                    $filterRange->setDateEnd($filterDateE);
                    if($filter instanceof filters\FilterDateBirth){
                        $filterAPI->setDateBirth($filterRange);
                    }
                    else{
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


        $data_encoded = curl_exec($ci);

        //var_dump(curl_getinfo($ci));
        //var_dump($data_encoded);

        if($isNbResults){
            $class = $this->getClassname("InternalInt32");
        }

        if($class !== null){
            if (curl_errno($ci)) {
                $data = new $class();
            } else {
                try{
                    if($this->output === self::OUTPUT_PB){
                        $data = new $class($data_encoded);
                        if($isNbResults){ $data = $data->getValue(); }
                    }
                    else{
                        $data = $data_encoded;
                    }
                }
                catch(\Exception $e){
                    if($this->output === self::OUTPUT_PB){
                        $data = new $class();
                        if($isNbResults){ $data = $data->getValue(); }
                    }
                    else{
                        $data = $data_encoded;
                    }

                }
                curl_close($ci);
            }
        }

        return $data;
    }

    /**
     * Prepare une requete à l'API de Geneweb
         *
     * @param string $type
     * @param mixed $object
     * @param string $class
         *
     * @return curl handler
     */
    protected function prepareRequest($type, $data, $class, $protoFilters){
        $vars = array();

        $vars['m'] = 'API_'.$type;
        $vars['input'] = 'pb';
        $vars['output'] = ($this->output == self::OUTPUT_BINARY) ? self::OUTPUT_PB : $this->output;
                $vars['sig'] = $this->signature;
        if($this->getIz() != ""){
            $vars['iz'] = $this->getIz();
        }
                $b = $this->user;
                // on vérifie que le mode magicien ou ami est activé
                $authorization = false;
                if($this->isModeWizard()){
                        $b .= "_w";
                        $authorization = true;
                }
                elseif($this->isModeFriend()){
                        $b .= "_f";
                        $authorization = true;
                }

                $vars["data"] = '';

        if($this->isFullInfos()){
            $vars['full_infos'] = $this->isFullInfos();
        }
        $isNbResults = false;

                $vars["filters"] = $protoFilters;

        $query = http_build_query($vars); //for GET
        $req = $this->url.$b."?".$query;

        $ci = curl_init();
        curl_setopt($ci, CURLOPT_URL, $req);
        curl_setopt($ci, CURLOPT_HEADER, false);
        curl_setopt($ci, CURLOPT_RETURNTRANSFER, true);
        //si le mode ami ou magicien est activé, on envoie les authentifications
        if($authorization){
                        curl_setopt($ci, CURLOPT_HTTPHEADER, array('Authorization: Basic '.base64_encode($this->user.":".$this->password)));
        }

                return array('c' => $ci, 'class'=> $class, 'base' => $this->user);
        }

        public function initFilter() {
        if(count($this->filters) > 0){
            $filterAPI = new \geneweb\api\object\Filters();
            foreach($this->filters AS $filter){
                if($filter instanceof filters\FilterOnlySosa){
                    $filterAPI->setOnlySosa(true);
                }
                elseif($filter instanceof filters\FilterOnlyRecent){
                    $filterAPI->setOnlyRecent(true);
                }
                elseif($filter instanceof filters\FilterSex){
                    $filterAPI->setSex($filter->getSex());
                }
                elseif($filter instanceof filters\FilterNbResults){
                    $filterAPI->setNbResults(true);
                    $isNbResults = true;
                }
                elseif(($filter instanceof filters\FilterDateBirth)
                        || ($filter instanceof filters\FilterDateDeath)){
                    $filterDateB = new \geneweb\api\object\FilterDate();
                    $filterDateB->setDay($filter->getDayBegin());
                    $filterDateB->setMonth($filter->getMonthBegin());
                    $filterDateB->setYear($filter->getYearBegin());
                    $filterDateE = new \geneweb\api\object\FilterDate();
                    $filterDateE->setDay($filter->getDayEnd());
                    $filterDateE->setMonth($filter->getMonthEnd());
                    $filterDateE->setYear($filter->getYearEnd());
                    $filterRange = new \geneweb\api\object\FilterDateRange();
                    $filterRange->setDateBegin($filterDateB)
    ;
                    $filterRange->setDateEnd($filterDateE);
                    if($filter instanceof filters\FilterDateBirth){
                        $filterAPI->setDateBirth($filterRange);
                    }
                    else{
                        $filterAPI->setDateDeath($filterRange);
                    }
                }
            }

            $fp = fopen('php://memory', 'r+b');
            $r = $filterAPI->write($fp);
            rewind($fp);
            $data = stream_get_contents($fp);
            fclose($fp);

                        return $data;
        }
                return null;
        }

    /**
     * Envoi de la requête à l'API de Geneweb
     * @param string $type
     * @param mixed $object
     * @param string $class
     * @return mixed
     */
    public function executeRequests($handlers){

                // On initialise une liste de sessions curl « $multihandler »
                // chacune correspondra à une instance de curl qui ira se connecter à une URL
                $multihandler = curl_multi_init();

                foreach ($handlers as $handler) {
                        curl_setopt($handler['c'], CURLOPT_RETURNTRANSFER, TRUE);
                        curl_multi_add_handle($multihandler, $handler['c']);
                }

                do {
                        curl_multi_exec($multihandler, $pendingConnex);
                        usleep(10000); // 10 ms
                } while ($pendingConnex > 0);


                // parse responses
                $result = array();
                foreach ($handlers as $handler) {
                        $data_encoded = curl_multi_getcontent($handler['c']);

                        try {
                                $class = $handler['class'];
                                $data = new $class($data_encoded);
                                $result[$handler['base']] = $data;
                        }
                        catch(\Exception $e){
                                $result[$handler['base']] = null;
                        }
                }

                // Fermeture des handlers : important pour éviter les fuites mémoires
                foreach ($handlers as $handler) {
                        curl_multi_remove_handle($multihandler, $handler['c']);
                }
                curl_multi_close($multihandler);

                return $result;
        }

        /*
         * Modifie l'url de l'instance GwAPI
         *
         * @param string $url
         *
         * @return void
         */
        public function setUrl($url) {
                $this->url = $url;
        }

        /*
         * Récupère l'url de l'instance GwAPI
         *
         * @return string
         */
        public function getUrl() {
                return $this->url;
        }
}

?>
