<?php
/**
 * PHP Client to test geneweb API
 *
 * @author Swan Desportes
 *
 */
require_once('GenewebSaisieReadAPI.php');
require_once('filters/Filter.cls.php');

//const URL_API = "http://192.168.1.150:2322/";
const URL_API = "http://vm-swan:2322/";

class Request {

    private static $instance = null;

    private function __construct() {
    }

    public static function getInstance() {

        if (is_null(self::$instance)) {
            self::$instance = new Request();
        }
        return self::$instance;
    }

    public function g($param) {
        if(!isset($_GET[$param])) return false;
        return $_GET[$param];
    }

    public function gp($param=null) {
        if(!isset($param)) return $_REQUEST;
        if(!isset($_REQUEST[$param])) return null;
        return $_REQUEST[$param];
    }

}


class APIController {

    // Last API call results
    protected $results = "";

    // API services exposed
    //protected $services = array("startingWithSearch", "approximativeSearch", "lastnameOrFirstnameSearch", "getIndividualInfos", "getAllPersons", "getAllFamilies", /* "getListIndividualInfos",*/ "updateImagePerson", "getImagePerson", "getBaseInfos", "getBaseWarnings","getMaxAncestors", "getLoopBase", "getAllImages", "getAllImagesInt", "getAllImagesExt", "getClosePersons", "getBirthdayPersons", "getGraphAscPerson", "getGraphDescPerson", "getGraphRelation", "getCplRelation");
    protected $services = array( "getTree","getTreeV2","getTreeFull", "getPerson");

    // Last API call request
    protected $request = "";

    public function run(){

        sort($this->services);

        $req = Request::getInstance();
        $results="";
        if($req->gp('base')){
            //$api = \geneanet\geneweb\api\genewebAPI::getInstance($req->gp('base'));
            $api = \Geneanet\Bundle\GenewebBundle\Api\SaisieRead\GenewebSaisieReadAPI::getInstance($req->gp('base'));
            $api->setUrl(URL_API);
            if($req->g('full') == 1){
                $api->setFullInfos(true);
            }
            switch($req->g('output')){
            case 'json':
                $api->setOutput('json');
                break;
            case 'xml':
                $api->setOutput('xml');
                break;
            }
            //$meta = meta::from_sourcename($req->gp('base'));
            switch($req->gp('type')){
            case 'friend':
                $api->enableModeFriend($req->gp('base'));
                break;
            case 'wizard':
                $api->enableModeWizard($req->gp('base'));
                break;
            }

            $filters = $req->gp('filters');
            foreach($filters AS $k => $filter){
                if(is_array($filter)) continue;
                $path = "filters/$k.cls.php";
                require_once("filters/$k.cls.php");
                $class = "\geneweb\\api\\filters\\".$k;
                $filter = new $class;
                if(isset($filters[$k."_val"])){
                    foreach($filters[$k."_val"] AS $k2 => $v2){
                        $set = "set".ucfirst($k2);
                        $filter->$set($v2);
                    }
                }
                $api->addFilter($filter);
            }
            $service = $req->gp('service');
            if($req->gp('p')){
                $results = call_user_func_array(array($api, $service), $req->gp('p'));
            }
            else{
                $results = call_user_func(array($api, $service));
            }
            if((string)$results == "") $results = "NULL";
            if( $req->g('output') == "xml"){
                $results = $this->xmlpp($results, true);
            }
            elseif($req->g('output') == "json"){
                $results = $this->jsonpp($results);
            }
            $this->request = $api->lastUrl;
        }
        $this->results = $results;
        echo "<style type='text/css'>
                        input[type='text'], input[type='submit'] {
                            border:1px solid gray;
                        }
                        .results {
                            border:1px solid gray;
                            background-color: #e9e9e9;
                            margin-bottom:40px;
                            padding-left:5px;
                        }

                        .results h2{
                            color:red;
                        }

                        .results pre {
                            height:400px;
                            overflow:auto;
                        }

                        .request {
                            border:1px solid gray;
                            background-color: #e9e9e9;
                            margin-bottom:40px;
                            padding-left:5px;
                        }

                        .request pre {
                            overflow:auto;
                        }

                        #selectbox {
                            position:fixed;
                            right:20px;
                        }
                        </style>";

    }

    /** Prettifies an XML string into a human-readable and indented work of art
     *  @param string $xml The XML as a string
     *  @param boolean $html_output True if the output should be escaped (for use in HTML)
     */
    private function xmlpp($xml, $html_output=false) {
        $xml_obj = new SimpleXMLElement($xml);
        $level = 4;
        $indent = 0; // current indentation level
        $pretty = array();

        // get an array containing each XML element
        $xml = explode("\n", preg_replace('/>\s*</', ">\n<", $xml_obj->asXML()));

        // shift off opening XML tag if present
        if (count($xml) && preg_match('/^<\?\s*xml/', $xml[0])) {
            $pretty[] = array_shift($xml);
        }

        foreach ($xml as $el) {
            if (preg_match('/^<([\w])+[^>\/]*>$/U', $el)) {
                // opening tag, increase indent
                $pretty[] = str_repeat(' ', $indent) . $el;
                $indent += $level;
            } else {
                if (preg_match('/^<\/.+>$/', $el)) {
                    $indent -= $level;  // closing tag, decrease indent
                }
                if ($indent < 0) {
                    $indent += $level;
                }
                $pretty[] = str_repeat(' ', $indent) . $el;
            }
        }
        $xml = implode("\n", $pretty);
        return ($html_output) ? htmlentities($xml, ENT_COMPAT | ENT_HTML401, 'UTF-8') : $xml;
    }


    function jsonpp ( $json ){
        $result = '';
        $level = 0;
        $prev_char = '';
        $in_quotes = false;
        $ends_line_level = NULL;
        $json_length = strlen( $json );

        for( $i = 0; $i < $json_length; $i++ ) {
            $char = $json[$i];
            $new_line_level = NULL;
            $post = "";
            if( $ends_line_level !== NULL ) {
                $new_line_level = $ends_line_level;
                $ends_line_level = NULL;
            }
            if( $char === '"' && $prev_char != '\\' ) {
                $in_quotes = !$in_quotes;
            } else if( ! $in_quotes ) {
                switch( $char ) {
                case '}': case ']':
                    $level--;
                    $ends_line_level = NULL;
                    $new_line_level = $level;
                    break;

                case '{': case '[':
                    $level++;
                case ',':
                    $ends_line_level = $level;
                    break;

                case ':':
                    $post = " ";
                    break;

                case " ": case "\t": case "\n": case "\r":
                    $char = "";
                    $ends_line_level = $new_line_level;
                    $new_line_level = NULL;
                    break;
                }
            }
            if( $new_line_level !== NULL ) {
                $result .= "\n".str_repeat( "\t", $new_line_level );
            }
            $result .= $char.$post;
            $prev_char = $char;
        }

        return $result;
    }

    public function render(){
        $req = \request::getInstance();

        // Services combo
        $content = "<div id='selectbox'><select onchange=\"window.location='#'+$(this).val();$(this).find('option[value=\'\']').attr('selected','selected');\"><option value=''>-- Choisissez une méthode --</option>";
        foreach($this->services AS $service){
            $content .= "<option value='".$service."'>".$service."()</option>";
        }
        $content .="</select></div>";

        // Request
        $content .= "<div class='request'><b>Request</b> : <pre>$this->request</pre></div>";

        // Results
        if($req->gp('base')){
            $content .="<div class='results'>";
            $content .="<h2>RESULTATS</h2>";
            $content .="<pre>$this->results</pre>";
            $content .="</div>";
        }

        $content .="<form>TYPE:";
        foreach($req->gp() AS $k => $v){
            if($k == "filters") continue;
            if(is_array($v)){
                foreach($v AS $k2 => $v2){
                    $content .="<input type='hidden' name='$k"."[".$k2."]' value=\"".htmlspecialchars($v2)."\">";
                }
            }
            elseif(!in_array($k, array("output", "full"))){
                $content .="<input type='hidden' name='$k' value=\"".htmlspecialchars($v)."\">";
            }
        }
        $content .="<label><input type='radio' name='output' value='' ".($req->gp('output') == "" ? "checked='checked'" : "")."/>object</label>
            <label><input type='radio' name='output' value='json' ".($req->gp('output') == "json" ? "checked='checked'" : "")."/>json</label>
            <label><input type='radio' name='output' value='xml' ".($req->gp('output') == "xml" ? "checked='checked'" : "")."/>xml</label><br>";
        $content .="<label><input type='checkbox' name='full' value='1' ".($req->gp('full') == "1" ? "checked='checked'" : "")."/>Infos complètes ?</label>";


        $directory = "filters/";
        $dir_r = opendir($directory) or die('Error filters directory');
        while(false !== ($entry = readdir($dir_r))){
            if(!is_dir($directory.'/'.$entry) && $entry != '.' && $entry != '..' && !preg_match("#(^\.|~$)#", $entry)) {
                require_once($directory.'/'.$entry);
                $class = str_replace(".cls.php", "", $entry);
                $rc = new ReflectionClass("\geneweb\\api\\filters\\".$class);
                if($rc->isAbstract()) continue;
                $methods = $rc->getMethods();
                $p = $req->gp("filters");
                $content .= "<br/><label><input type='checkbox' name='filters[$class]' value='1' ".($p[$class] == "1" ? "checked='checked'" : "")." ".((count($methods) > 1) ? "onclick=\"($(this).is(':checked') ? $(this).parent().next().show() : $(this).parent().next().hide())\"" : "")."/>".$class."</label>";
                $methods = $rc->getMethods();
                $defaultProperties = $rc->getDefaultProperties();
                if(count($methods) > 1){
                    $content .= "<div".($p[$class] != "1" ? " style='display:none'" : "")."><table>";
                    foreach($methods AS $method){
                        if($method->name == "__construct" || preg_match("#^get#", $method->name)) continue;
                        $p = $req->gp("filters");
                        if($p){
                            $p = $p[$class."_val"];
                        }
                        $name = lcfirst(preg_replace("#set#", "", $method->name));
                        $val = $p[$name];
                        if(empty($val)){
                            $default = $defaultProperties[$name];
                            if($default !== null){
                                $val = $default;
                            }
                        }

                        $content .= "<tr><td style='text-align:right;'><label style='margin-left:50px;'>".$name."</label>:&nbsp;</td><td><input type='text' name='filters[".$class."_val][$name]' value='".htmlspecialchars($val)."' size='5'></td></tr>";
                    }
                    $content .= "</table></div>";
                }

            }
        }
        closedir($dir_r);

        $content .= "<br/><br/><input type='submit' value='submit'/>
            </form>";

        foreach($this->services AS $service){
            $content .="<a name='$service'></a><p><h2 style='font-size:24px;font-weight:bold;'>$service</h2>";
            $reflector = new ReflectionClass('\Geneanet\Bundle\GenewebBundle\Api\SaisieRead\GenewebSaisieReadAPI');
            $method = $reflector->getMethod($service);
            $content .="<pre>\n\t".$method->getDocComment()."\n</pre>\n";
            $content .="<form action='?output=".$req->gp('output')."&full=".$req->gp('full')."' method='post'>\n<input type='hidden' name='service' value='$service'>\n";
            $content .=" <label><input type='radio' name='type' value='friend' ".($req->gp('type') == "friend" ? "checked='checked'" : "").">Friend</label>\n";
            $content .=" <label><input type='radio' name='type' value='wizard' ".($req->gp('type') == "wizard" ? "checked='checked'" : "").">Wizard</label>\n";
            $content .=" <label><input type='radio' name='type' value='' ".($req->gp('type') == "" ? "checked='checked'" : "").">None</label>\n";
            $filters = $req->gp('filters');
            foreach($filters AS $k => $filter){
                if(is_array($filter)){
                    foreach($filter AS $k2 => $v2){
                        $content .= "<input type='hidden' name='filters[$k][$k2]' value=\"".htmlspecialchars($v2)."\"/>";
                    }
                    continue;
                }
                $content .= "<input type='hidden' name='filters[$k]' value=\"".htmlspecialchars($filter)."\"/>";

            }
            $content .=" <br/>\n";

            $i=0;
            $content .=" <table>\n";
            $content .="  <tr>\n   <td align='right'>base:&nbsp;</td>\n   <td><input type='text' name='base' value='".$req->gp('base')."'></td>\n  </tr>\n";
            foreach($method->getParameters() AS $parameter){
                $p = $req->gp('p');
                $key = "input".md5(uniqid("", true));
                $content .="  <tr>\n   <td align='right'><label for='$key'>$parameter->name:&nbsp;</label></td>\n   <td><input type='text' id='$key' name='p[".$parameter->name."]' value=\"".htmlspecialchars($p[$parameter->name])."\"></td>\n  </tr>\n";
                //                $content .=$parameter->name;
                //                $content .="\n";
                $i++;
            }
            $content .="  <tr>\n   <td></td>\n   <td align='right'><input type='submit' value='submit'></td>\n  </tr>\n";
            $content .=" </table>\n";
            $content .="</form>\n";
            $content .="<hr/>\n";
        }
        return $content;
    }

    public function enumCss(){
    }

    public function enumJs(){
    }

}
?><html>
<head>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/jquery/2.1.4/jquery.min.js"></script>
<?
$controller = new APIController();
$controller->run();
?>
</head>
<body>
<?
echo $controller->render();
?>
</body>
</html>
