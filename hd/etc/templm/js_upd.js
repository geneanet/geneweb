/* $Id: js_upd.js,v 7.00 2016/03/29 22:37:43 mr Exp $ */
  function oKP1(event)
  {
    var key = event.keyCode ? event.keyCode : event.which ? event.which : event.charCode;
    var charKey = String.fromCharCode(key).toUpperCase();
    var r = /[MFN]/; /* lettres choisies */
    var t = r.test(charKey);
    if (t == true)
    {
      var e1 = document.getElementById("sex1");
      var e2 = document.getElementById("first_name");
      switch(charKey)
      {
        case "M": e1.value = "M"; e2.select(); break;
        case "F": e1.value = "F"; e2.select(); break;
        case "N": e1.value = "N"; e2.select(); break;
      }
      return false;
    }
  }
  function oKP2(event,z2,z3,z4,z5)
  {
    var t1 = (z2 == "witn") ? "e" + z5 + "_witn": z2;
    var key = event.keyCode ? event.keyCode : event.which ? event.which : event.charCode;
    var charKey = String.fromCharCode(key).toUpperCase();
    if (z2 == "ch" || z2 == "witn")
    {
      var r = /[MFNP]/; /* lettres choisies */
      var t = r.test(charKey);
      if (t == true)
      {
        var e1 = document.getElementById(t1 + z3 + "_occ");
        var e2 = document.getElementById(t1 + z3 + "_fn");
        var a1 = (e1.value == "") ? e1.getAttribute("placeHolder","false"): e1.value;
        switch(charKey)
        {
          case "M": e1.value = "M"; e2.select(); break;/* masculin */
          case "F": e1.value = "F"; e2.select(); break;/* féminin */
          case "N": e1.value = "N"; e2.select(); break;/* neutre */
          case "P": if(z2 == "ch")visHid(t1,'_sn'); break;/* afficher/masquer patronyme de l'enfant */
        }
        return false;
      }
    }
    var r = /[RDIAHBX]/; /* lettres choisies */
    var t = r.test(charKey);
    if (t == true)
    {
      switch(charKey)
      {
        case "R": invertS(z2,z3,z4,0,1,z5); break;/* remonter (permuter 3 et 2) */
        case "D": invertS(z2,z3,z4,1,1,z5); break;/* descendre (permuter 4 et 3) */
        case "I": addS(z2,z3,0,z5); break;/* insérer une ligne sur place */
        case "A": addS(z2,z3,1,z5); break;/* ajouter une ligne à la fin */
        case "H": invertS(z2,z3,z4,0,0,z5); break;/* haut (position sur la ligne précédente) */
        case "B": invertS(z2,z3,z4,1,0,z5); break;/* bas (position sur la ligne suivante) */
        case "X": delS(z2,z3,z4,z5); break;/* supprimer */
      }
      return false;
    }
    if (z2 == "r")
    {
      var r = /[VWXYZ]/; /* lettres choisies */
      var t = r.test(charKey);
      if (t == true)
      {
        var n = document.getElementsByName(z2 + z3 + "_type")[0];
        switch(charKey)
        {
          case "V": n.options[0].selected = true; break;/* Parrain et Marraine */
          case "W": n.options[1].selected = true; break;/* Parents adoptifs */
          case "X": n.options[2].selected = true; break;/* Parents l’ayant reconnu */
          case "Y": n.options[3].selected = true; break;/* Parents possibles */
          case "Z": n.options[4].selected = true; break;/* Parents nourriciers */
        }
        return false;
      }
    }
  }
  function oKP3(event,e)
  {
    var key = event.keyCode ? event.keyCode : event.which ? event.which : event.charCode;
    var charKey = String.fromCharCode(key).toUpperCase();
    var e1 = e.nextSibling;
    var e2 = e1.nextSibling;
    if (charKey == "+")
    {
      e.value = "#deat";
      e2.focus();
      return false;
    }
  }
  function accent0()
  {
    var e1=document.getElementById('notes');
    e1.style.width="415px";
    e1.style.height="270px";
    document.getElementById('accent').style.display="none";
    document.getElementById('accent0').style.display="none";
    document.getElementById('accent1').style.display="block";
    document.getElementById('notes1').style.width="420px";
    document.getElementById('accent2').style.width="auto";
    window.scrollBy(0,-300);
    e1.focus();
  }
  function accent1()
  {
    var e1=document.getElementById('notes');
    e1.style.width="99%";
    e1.style.height="400px";
    document.getElementById('accent').style.display="block";
    document.getElementById('accent0').style.display="block";
    document.getElementById('accent1').style.display="none";
    document.getElementById('notes1').style.width="99%";
    document.getElementById('accent2').style.width="98%";
    window.scrollBy(0,300);
    e1.focus();
  }
  function itemMaxCnt(item,cnt)
  {
    while (document.getElementById(item + cnt)){cnt++}
    return cnt;
  }
  function addItem(item,cnt,lex)
  {
    var cnt1 = itemMaxCnt(item,cnt);
    var cnt2 = cnt1 + 1;
    var id0 = item + cnt1;
    var id1 = "new_" + item;
    document.getElementById(id1).outerHTML = '\
    <label>' + lex + ' ' + cnt2 + '<input id="' + id0 + '" name="' + id0 + '" size="30" value=""\/><\/label><a href="javascript:addItem(\'' + item + '\',' + cnt2 + ',\''+ lex + '\')"> + <\/a>\n\
    <span id="' + id1 + '"><\/span>';
    for (var i = cnt1; i > cnt; i--)
    {
      invertItem(item,i);
    }
    document.getElementById(item + cnt).focus();
  }
  function invertItem(item,cnt)
  {
    if (cnt > 0)
    {
      var cnt1 = cnt - 1;
      var v1 = document.getElementById(item + cnt1).value; 
      var v2 = document.getElementById(item + cnt).value;
      document.getElementById(item + cnt1).value = v2;
      document.getElementById(item + cnt).value = v1;
    }
  }
  function addEvent(z1,z2)
  {
    var c0 = z1;
    var c1 = itemMaxCnt('e',c0);
    var c2 = c1 + 1;
    var c3 = c1 - 1;
    var dc1 = "e_date" + c1;
    var d = dmyDate(dc1);
    var hd = hDate(dc1);
    var v1 = document.getElementById("e_place" + c3).value;
    var t1 = document.getElementById("aw1").firstChild.data;
    document.getElementById("new_event").outerHTML = '\
      <dl id="e'+ c1 +'">\n\
      <dt>\n\
      <input id="e_name'+ c1 +'" name="e_name'+ c1 +'" value="" type="hidden">\n\
      <input id="e' + c1 + '_occ" name="e' + c1 + '_occ" class="e_occ" size=1 maxlength=1 placeholder="#"\n\
      onkeypress="javascript:return oKP2(event,\'e\',' + c1 + ',\'_occ\',\'\')" onblur="this.value=\'\';"\n\
      /><input id="e_name'+ c1 +'_dl" name="e_name'+ c1 +'_dl" class="e" size="20" maxlength="200" value=""\n\
      list="dlevent" autocomplete="off"\n\
      onkeypress="javascript:return oKP3(event,this)" onblur="oB6(\''+ c1 +'\')"\n\
      onkeydown="if(event.keyCode == 13)oB6(\''+ c1 +'\')"\n\
      ><code>   <\/code>'+ d +'\n\
      '+ hd +'\n\
      <input id="e_place'+ c1 +'" name="e_place'+ c1 +'" class="pl" size="70" value="'+ v1 +'"\n\
      ' + g_place + '><\/dt>\n\
      <dd><textarea id="e_note'+ c1 +'" name="e_note'+ c1 +'" class="enote"><\/textarea><\/dd>\n\
      <dd><input id="e_src'+ c1 +'" name="e_src'+ c1 +'" class="esrc" value=""' + g_src + '><\/dd>\n\
      <dd id="new_e'+ c1 +'_witn"><\/dd>\n\
      <dd><a href="javascript:addWitness(1,1,'+ c1 +')">'+ t1 +'<\/a><\/dd>\n\
      <\/dl>\n\
      <dl id="new_event"></dl>';
    if(z2 == 0)
    {
      for(var i = c1; i > c0; i--){invertEvent(i,c3);}
      sIV("e_name" + c0 + "_dl");
    }
    else sIV("e_name" + c1 + "_dl");
  }
  function invertEvent(cnt)
  {
    if (cnt > 0)
    {
      var t0 = cnt - 1;
      var t1 = cnt;
      var a = new Array();
      a[0] = new Array();
      a[0]["e10"]= "e_name" + t0;
      a[0]["e11"]= "e" + t0 + "_occ";
      a[0]["e12"]= "e_name" + t0 + "_dl";
      a[0]["e13"]= "e_date" + t0 + "_dd";
      a[0]["e14"]= "e_date" + t0 + "_mm";
      a[0]["e15"]= "e_date" + t0 + "_yy";
      a[0]["e16"]= "e_date" + t0 + "_cal";
      a[0]["e17"]= "e_date" + t0 + "_yyyy";
      a[0]["e18"]= "e_date" + t0 + "_oryear";
      a[0]["e19"]= "e_date" + t0 + "_ormonth";
      a[0]["e20"]= "e_date" + t0 + "_orday";
      a[0]["e21"]= "e_date" + t0 + "_text";
      a[0]["e22"]= "e_date" + t0 + "_prec";
      a[0]["e23"]= "e_place" + t0;
      a[0]["e24"]= "e_note" + t0;
      a[0]["e25"]= "e_src" + t0;

      a[1] = new Array();
      a[1]["e10"]= "e_name" + t1;
      a[1]["e11"]= "e" + t1 + "_occ";
      a[1]["e12"]= "e_name" + t1 + "_dl";
      a[1]["e13"]= "e_date" + t1 + "_dd";
      a[1]["e14"]= "e_date" + t1 + "_mm";
      a[1]["e15"]= "e_date" + t1 + "_yy";
      a[1]["e16"]= "e_date" + t1 + "_cal";
      a[1]["e17"]= "e_date" + t1 + "_yyyy";
      a[1]["e18"]= "e_date" + t1 + "_oryear";
      a[1]["e19"]= "e_date" + t1 + "_ormonth";
      a[1]["e20"]= "e_date" + t1 + "_orday";
      a[1]["e21"]= "e_date" + t1 + "_text";
      a[1]["e22"]= "e_date" + t1 + "_prec";
      a[1]["e23"]= "e_place" + t1;
      a[1]["e24"]= "e_note" + t1;
      a[1]["e25"]= "e_src" + t1;

      for (var i=10; i<=25; i++)
      {
        var v1 = document.getElementById(a[0]["e" + i]).value; 
        var v2 = document.getElementById(a[1]["e" + i]).value;
        document.getElementById(a[0]["e" + i]).value = v2;
        document.getElementById(a[1]["e" + i]).value = v1;
      }
      invertEventWitness(cnt);
    }
  }
  function invertEventWitness(cnt)
  {
    if (cnt > 0)
    {
      var c1 = cnt - 1;
      var c2 = cnt;
      var i = 1;
      while(i > 0)
      {
        var t1 = "e" + c1 + "_witn" + i + "_fn";
        var t2 = "e" + c2 + "_witn" + i + "_fn";
        var e1 = (document.getElementById(t1)) ? document.getElementById(t1): "";
        var e2 = (document.getElementById(t2)) ? document.getElementById(t2): "";

        if (e1 != "" || e2 != "")
        {
          if (e1 == "" && e2 != ""){addWitness(1,1,c1)}
          if (e1 != "" && e2 == ""){addWitness(1,1,c2)}
          invertWitness(i,0,c2);
          i++;
        }
        else
        {
          i--;
          while (i > 0)
          {
            var t1 = "e" + c1 + "_witn" + i + "_fn";
            var t2 = "e" + c2 + "_witn" + i + "_fn";
            var v1 = (document.getElementById(t1)) ? document.getElementById(t1).value: "false";
            var v2 = (document.getElementById(t2)) ? document.getElementById(t2).value: "false";
            if (i > 1)
            {    
              if (v1 == ""){delS('witn',i,'_occ',c1)}
              if (v2 == ""){delS('witn',i,'_occ',c2)}
            }
            i--;
            if (v1 == "false" && v2 == "false"){i = 0}
          }
          i = 0;
        }
      }
    }
  }
  function addRelation(z1,z2)
  {
    var c0 = z1;
    var c1 = itemMaxCnt('r',c0);
    var v = document.getElementById("r1_fath_fn").value;
    var v1 = document.getElementById("r1_moth_fn").value;
    if(v != "" || v1 != "")
    {
      var t0 = "r" + c0;
      var t1 = "r" + c1;
      var t1f = t1 + "_fath";
      var t1m = t1 + "_moth";
      var l2 = document.getElementsByName("r1_type")[0].options[0].firstChild.data;
      var l3 = document.getElementsByName("r1_type")[0].options[1].firstChild.data;
      var l4 = document.getElementsByName("r1_type")[0].options[2].firstChild.data;
      var l5 = document.getElementsByName("r1_type")[0].options[3].firstChild.data;
      var l6 = document.getElementsByName("r1_type")[0].options[4].firstChild.data;
      var l12 = document.getElementById("rM").firstChild.data;
      var l13 = document.getElementById("rF").firstChild.data;
      document.getElementById("new_relation").outerHTML = '\
      <tr id="' + t1 + '">\n\
        <td><input type="hidden" id="' + t1f + '_p" name="' + t1f + '_p" value="create">\n\
            <input id="' + t1f + '_occ" name="' + t1f + '_occ" class="occ0" autocomplete="off" placeholder="' + l12 + '" size="3" maxlength="8" value="" onkeypress="javascript:return oKP2(event,\'r\',' + c1 + ',\'_fath_occ\',\'\')" onblur="oB3(\'' + t1f + '\')"><\/td>\n\
        <td><input id="' + t1f + '_fn" name="' + t1f + '_fn" class="fn ar" size="30" maxlength="200" value="" onblur="tUC1(this)" onkeydown="if(event.keyCode == 13)tUC1(this)" ' + g_fn + '><\/td>\n\
        <td><input id="' + t1f + '_sn" name="' + t1f + '_sn" class="sn" size="30" maxlength="200" value="" onblur="tUC(this);jq1(\''+ t1f +'\',\'0\')" onkeydown="if(event.keyCode == 13)tUC(this)" ' + g_sn + '><\/td>\n\
        <td><span id="'+ t1f +'_jq1"> <\/span><\/td>\n\
        <td rowspan="2" class="bg7"><select id="' + t1 + '_type" name="' + t1 + '_type">\n\
        <option value="GodParent" selected="selected">' + l2 + '<\/option>\n\
        <option value="Adoption">' + l3 + '<\/option>\n\
        <option value="Recognition">' + l4 + '<\/option>\n\
        <option value="CandidateParent">' + l5 + '<\/option>\n\
        <option value="FosterParent">' + l6 + '<\/option>\n\
        <\/select><\/td>\n\
        <\/tr>\n\
        <tr>\n\
        <td><input type="hidden" id="' + t1m + '_p" name="' + t1m + '_p" value="create">\n\
            <input id="' + t1m + '_occ" name="' + t1m + '_occ" class="occ1" autocomplete="off" placeholder="' + l13 + '" size="3" maxlength="8" value="" onkeypress="javascript:return oKP2(event,\'r\',\'' + c1 + '\',\'_moth_occ\',\'\')" onblur="oB3(\'' + t1m + '\')"><\/td>\n\
        <td><input id="' + t1m + '_fn" name="' + t1m + '_fn" class="fn ar" size="30" maxlength="200" value="" onblur="tUC1(this)" onkeydown="if(event.keyCode == 13)tUC1(this)" ' + g_fn + '><\/td>\n\
        <td><input id="' + t1m + '_sn" name="' + t1m + '_sn" class="sn" size="30" maxlength="200" value="" onblur="tUC(this);jq1(\''+ t1m +'\',\'1\')" onkeydown="if(event.keyCode == 13)tUC(this)" ' + g_sn + '><\/td>\n\
        <td><span id="'+ t1m +'_jq1"> <\/span><\/td>\n\
      <\/tr>\n\
      <tr id="new_relation"><\/tr>';
      if(z2 == 0)
      {
        for(var i = c1; i > c0; i--){invertRelation(i);}
        sIV(t0 + "_fath_occ");
      }
      else sIV(t1 + "_fath_occ");
    }
    else sIV("r1_fath_occ");
  }
  function invertRelation(cnt)
  {
    if (cnt > 0)
    {
      var t0 = "r" + (cnt - 1);
      var t1 = "r" + cnt;
      var a = new Array();
      
      a[0] = new Array();
      a[0]["e10"]= t0 + "_type";
      a[0]["e11"]= t0 + "_fath_fn";
      a[0]["e12"]= t0 + "_fath_sn";
      a[0]["e13"]= t0 + "_fath_occ";
      a[0]["e14"]= t0 + "_moth_fn";
      a[0]["e15"]= t0 + "_moth_sn";
      a[0]["e16"]= t0 + "_moth_occ";
      a[0]["e17"]= t0 + "_fath_p";
      a[0]["e18"]= t0 + "_moth_p";
      a[0]["e19"]= t0 + "_fath_jq1";
      a[0]["e20"]= t0 + "_moth_jq1";
      
      a[1] = new Array();
      a[1]["e10"]= t1 + "_type";
      a[1]["e11"]= t1 + "_fath_fn";
      a[1]["e12"]= t1 + "_fath_sn";
      a[1]["e13"]= t1 + "_fath_occ";
      a[1]["e14"]= t1 + "_moth_fn";
      a[1]["e15"]= t1 + "_moth_sn";
      a[1]["e16"]= t1 + "_moth_occ";
      a[1]["e17"]= t1 + "_fath_p";
      a[1]["e18"]= t1 + "_moth_p";
      a[1]["e19"]= t1 + "_fath_jq1";
      a[1]["e20"]= t1 + "_moth_jq1";
      
      var n0 = document.getElementsByName(a[0]["e10"])[0];
      var n1 = document.getElementsByName(a[1]["e10"])[0];
      for(var i = 0; i < n0.length; i++)
      {
        var v0 = n0.options[i].selected;
        var v1 = n1.options[i].selected;
        if (v0 == true)
          var v0true = n1.options[i];
        if (v1 == true)
          var v1true = n0.options[i];
      }
      v0true.selected = true;
      v1true.selected = true;
         
      for (var i = 11; i <= 18; i++)
      {
        var v0 = document.getElementById(a[0]["e" + i]).value;
        var v1 = document.getElementById(a[1]["e" + i]).value;
        document.getElementById(a[0]["e" + i]).value = v1;
        document.getElementById(a[1]["e" + i]).value = v0;
      }
      for (var i = 19; i <= 20; i++)
      {
        var e0 = document.getElementById(a[0]["e" + i]);
        var v0 = e0.firstChild ? e0.firstChild.data : "";
        var e1 = document.getElementById(a[1]["e" + i]);
        var v1 = e1.firstChild ? e1.firstChild.data : "";
        document.getElementById(a[0]["e" + i]).innerHTML = v1;
        document.getElementById(a[1]["e" + i]).innerHTML = v0;
        var v0 = e0.title;
        var v1 = e1.title;
        document.getElementById(a[0]["e" + i]).title = v1;
        document.getElementById(a[1]["e" + i]).title = v0;
      }
    }
  }
  function dmyDate(z1)
  {
      var dmy = document.getElementById("dmy").firstChild.data;
      if (dmy == "ddmmyyyy")
      {
        var d ="\
<input id=" + z1 + "_dd name=" + z1 + "_dd class=d autocomplete=off size=1 maxlength=2 value=\"\" onkeypress=\"javascript:return oKPdd(event,'" + z1 + "','_dd','_mm');\"\/>\
<input id=" + z1 + "_mm name=" + z1 + "_mm class=m autocomplete=off size=1 maxlength=2 value=\"\" list=\"dlmonth\" onkeypress=\"javascript:return oKPmm(event,'" + z1 + "','_mm','_yy');\" onkeydown=\"javascript:return oKD1(event,'" + z1 + "_mm','" + z1 + "_dd');\"\/>\
<input id=" + z1 + "_yy name=" + z1 + "_yy class=y autocomplete=off size=12 maxlength=50 value=\"\" onkeydown=\"if(event.keyCode == 13)javascript:evD('" + z1 + "');return oKD1(event,'" + z1 + "_yy','" + z1 + "_mm');\" onblur=\"javascript:evD('" + z1 + "')\"\/>"
      }
      if (dmy == "mmddyyyy")
      {
        var d ="\
<input id=" + z1 + "_mm name=" + z1 + "_mm class=m autocomplete=off size=1 maxlength=2 value=\"\" list=\"dlmonth\" onkeypress=\"javascript:return oKPmm(event,'" + z1 + "','_mm','_dd');\"\/>\
<input id=" + z1 + "_dd name=" + z1 + "_dd class=d autocomplete=off size=1 maxlength=2 value=\"\" onkeypress=\"javascript:return oKPdd(event,'" + z1 + "','_dd','_yy');\" onkeydown=\"javascript:return oKD1(event,'" + z1 + "_dd','" + z1 + "_mm');\"\/>\
<input id=" + z1 + "_yy name=" + z1 + "_yy class=y autocomplete=off size=12 maxlength=50 value=\"\" onkeydown=\"if(event.keyCode == 13)javascript:evD('" + z1 + "');return oKD1(event,'" + z1 + "_yy','" + z1 + "_dd');\" onblur=\"javascript:evD('" + z1 + "')\"\/>"
      }
      if (dmy == "yyyymmdd")
      {
        var d ="\
<input id=" + z1 + "_yy name=" + z1 + "_yy class=y autocomplete=off size=12 maxlength=50 value=\"\" onkeypress=\"javascript:return cF1(event,'" + z1 + "_mm');\" onkeydown=\"if(event.keyCode == 13)javascript:evD('" + z1 + "')\" onblur=\"javascript:evD('" + z1 + "')\"\/>\
<input id=" + z1 + "_mm name=" + z1 + "_mm class=m autocomplete=off size=1 maxlength=2 value=\"\" list=\"dlmonth\" onkeypress=\"javascript:return oKPmm(event,'" + z1 + "','_mm','_dd');\" onkeydown=\"javascript:return oKD1(event,'" + z1 + "_mm','" + z1 + "_yy');\"\/>\
<input id=" + z1 + "_dd name=" + z1 + "_dd class=d autocomplete=off size=1 maxlength=2 value=\"\" onkeypress=\"javascript:return oKPdd(event,'" + z1 + "','_dd','_place');\" onkeydown=\"javascript:return oKD1(event,'" + z1 + "_dd','" + z1 + "_mm');\"\/>"
      }
      return d;
  }
  function hDate(z1)
  {
    var d ="\n\
      <input type=hidden id=" + z1 + "_cal name=" + z1 + "_cal value=\"\" \/>\n\
      <input type=hidden id=" + z1 + "_yyyy name=" + z1 + "_yyyy value=\"\" \/>\n\
      <input type=hidden id=" + z1 + "_oryear name=" + z1 + "_oryear value=\"\" \/>\n\
      <input type=hidden id=" + z1 + "_ormonth name=" + z1 + "_ormonth value=\"\" \/>\n\
      <input type=hidden id=" + z1 + "_orday name=" + z1 + "_orday value=\"\" \/>\n\
      <input type=hidden id=" + z1 + "_text name=" + z1 + "_text value=\"\" \/>\n\
      <input type=hidden id=" + z1 + "_prec name=" + z1 + "_prec value=\"\" \/>"
    return d;
  }
  function addTitle(z1,z2)
  {
    var c0 = z1;
    var c1 = itemMaxCnt('t',c0);
    var sc1 = "t_date_start" + c1;
    var ec1 = "t_date_end" + c1;
    var v = document.getElementById("t_ident1").value;
    if(v != "")
    {
      var d_start = dmyDate(sc1);
      var hd_start = hDate(sc1);
      var d_end = dmyDate(ec1);
      var hd_end = hDate(ec1);
      document.getElementById("new_title").outerHTML = '\
      <tr id="t' + c1 + '">\n\
      <td class="bg7"><input id="t' + c1 + '_occ" name="t' + c1 + '_occ" class="t_occ" size=1 maxlength=1 placeholder="#" onkeypress="javascript:return oKP2(event,\'t\',' + c1 + ',\'_occ\',\'\')" onblur="this.value=\'\';"/></td>\n\
      <td><input id="t_ident' + c1 + '" name="t_ident' + c1 + '" size="30" value=""<\/td>\n\
      <td><input id="t_place' + c1 + '" name="t_place' + c1 + '" size="30" value=""><\/td>\n\
      <td><input id="t_name' + c1 + '" name="t_name' + c1 + '" size="30" value=""><\/td>\n\
      <td><input autocomplete="off" class="number" id="t_nth' + c1 + '" name="t_nth' + c1 + '" size="3" value=""><\/td>\n\
      <td><span class="dmyt">' + d_start + '<\/span><span class="dmyt">' + d_end + '<\/span><\/td>\n\
      ' + hd_start + '\n\
      ' + hd_end + '\n\
      <\/tr>\n\
      <tr id="new_title"><\/tr>';
      if(z2 == 0)
      {
        for (var i = c1; i > c0; i--)
        {
          invertTitle(i);
        }
        sIV("t_ident" + c0);
      }
      else sIV("t_ident" + c1);
    }
    else sIV("t_ident1");
  }
  function invertTitle(cnt)
  {
    if (cnt > 1)
    {
     var c0 = cnt - 1;
     var sc0 = "t_date_start" + c0;
     var ec0 = "t_date_end" + c0;
     var c1 = cnt;
     var sc1 = "t_date_start" + c1;
     var ec1 = "t_date_end" + c1;
     var a = new Array();

     a[0] = new Array();
     a[0]["e10"] = "t_ident" + c0;
     a[0]["e11"] = "t_place" + c0;
     a[0]["e12"] = "t_name" + c0;
     a[0]["e13"] = "t_nth" + c0;
     a[0]["e14"] = sc0 + "_cal";
     a[0]["e15"] = sc0 + "_oryear";
     a[0]["e16"] = sc0 + "_text";
     a[0]["e17"] = sc0 + "_prec";
     a[0]["e18"] = sc0 + "_dd";
     a[0]["e19"] = sc0 + "_mm";
     a[0]["e20"] = sc0 + "_yy";
     a[0]["e21"] = sc0 + "_yyyy";
     a[0]["e22"] = ec0 + "_cal";
     a[0]["e23"] = ec0 + "_oryear";
     a[0]["e24"] = ec0 + "_text";
     a[0]["e25"] = ec0 + "_prec";
     a[0]["e26"] = ec0 + "_dd";
     a[0]["e27"] = ec0 + "_mm";
     a[0]["e28"] = ec0 + "_yy";
     a[0]["e29"] = ec0 + "_yyyy";
    
     a[1] = new Array();
     a[1]["e10"] = "t_ident" + c1;
     a[1]["e11"] = "t_place" + c1;
     a[1]["e12"] = "t_name" + c1;
     a[1]["e13"] = "t_nth" + c1;
     a[1]["e14"] = sc1 + "_cal";
     a[1]["e15"] = sc1 + "_oryear";
     a[1]["e16"] = sc1 + "_text";
     a[1]["e17"] = sc1 + "_prec";
     a[1]["e18"] = sc1 + "_dd";
     a[1]["e19"] = sc1 + "_mm";
     a[1]["e20"] = sc1 + "_yy";
     a[1]["e21"] = sc1 + "_yyyy";
     a[1]["e22"] = ec1 + "_cal";
     a[1]["e23"] = ec1 + "_oryear";
     a[1]["e24"] = ec1 + "_text";
     a[1]["e25"] = ec1 + "_prec";
     a[1]["e26"] = ec1 + "_dd";
     a[1]["e27"] = ec1 + "_mm";
     a[1]["e28"] = ec1 + "_yy";
     a[1]["e29"] = ec1 + "_yyyy";

     for (var i=10; i<=29; i++)
     {
       var v1 = document.getElementById(a[0]["e" + i]).value; 
       var v2 = document.getElementById(a[1]["e" + i]).value;
       document.getElementById(a[0]["e" + i]).value = v2;
       document.getElementById(a[1]["e" + i]).value = v1;
     }
    }
  }
  function addChild(z1,z2)
  {
    var c0 = z1;
    var c1 = itemMaxCnt('ch',c0);
    var v = document.getElementById("ch1_fn").value;
    if(v != "")
    {
      var e2 = document.getElementById("ch1_sn");
      var t0 = "ch" + c0;
      var t1 = "ch" + c1;
      var v1 = document.getElementById("pa1_sn").value;
      var v2 = e2.type;
      var v3 = e2.value == "?" ? document.getElementById("pa2_sn").value : e2.value;
      var dmy = document.getElementById("dmy").firstChild.data;
      if (dmy == "ddmmyyyy")
      {
        var d_b ="\
<input id=" + t1 + "b_dd name=" + t1 + "b_dd class=d autocomplete=off size=1 maxlength=2 value=\"\" onkeypress=\"javascript:return oKPdd(event,'" + t1 + "b','_dd','_mm');\"\/>\
<input id=" + t1 + "b_mm name=" + t1 + "b_mm class=m autocomplete=off size=1 maxlength=2 value=\"\" list=\"dlmonth\" onkeypress=\"javascript:return oKPmm(event,'" + t1 + "b','_mm','_yy');\" onkeydown=\"javascript:return oKD1(event,'" + t1 + "b_mm','" + t1 + "b_dd');\"\/>\
<input id=" + t1 + "b_yy name=" + t1 + "b_yyyy class=ys autocomplete=off size=4 maxlength=12 value=\"\" onkeydown=\"if(event.keyCode == 13)javascript:evSD('" + t1 + "b');return oKD1(event,'" + t1 + "b_yy','" + t1 + "b_mm');\" onkeypress=\"javascript:return cF2(event,\'ch\'," + c1 + ",\'b_yy\');\" onblur=\"javascript:evSD('" + t1 + "b')\"\/>"
        var d_d ="\
<input id=" + t1 + "d_dd name=" + t1 + "d_dd class=d autocomplete=off size=1 maxlength=2 value=\"\" onkeypress=\"javascript:return oKPdd(event,'" + t1 + "d','_dd','_mm');\"\/>\
<input id=" + t1 + "d_mm name=" + t1 + "d_mm class=m autocomplete=off size=1 maxlength=2 value=\"\" list=\"dlmonth\" onkeypress=\"javascript:return oKPmm(event,'" + t1 + "d','_mm','_yy');\" onkeydown=\"javascript:return oKD1(event,'" + t1 + "d_mm','" + t1 + "d_dd');\"\/>\
<input id=" + t1 + "d_yy name=" + t1 + "d_yyyy class=ys autocomplete=off size=4 maxlength=12 value=\"\" onkeydown=\"if(event.keyCode == 13)javascript:evSD('" + t1 + "d');return oKD1(event,'" + t1 + "d_yy','" + t1 + "d_mm');\" onkeypress=\"javascript:return cF2(event,\'ch\'," + c1 + ",\'d_yy\');\" onblur=\"javascript:evSD('" + t1 + "d')\"\/>"
      }
      if (dmy == "mmddyyyy")
      {
        var d_b ="\
<input id=" + t1 + "b_mm name=" + t1 + "b_mm class=m autocomplete=off size=1 maxlength=2 value=\"\" list=\"dlmonth\" onkeypress=\"javascript:return oKPmm(event,'" + t1 + "b','_mm','_dd');\"\/>\
<input id=" + t1 + "b_dd name=" + t1 + "b_dd class=d autocomplete=off size=1 maxlength=2 value=\"\" onkeypress=\"javascript:return oKPdd(event,'" + t1 + "b','_dd','_yy');\" onkeydown=\"javascript:return oKD1(event,'" + t1 + "b_dd','" + t1 + "b_mm');\"\/>\
<input id=" + t1 + "b_yy name=" + t1 + "b_yyyy class=ys autocomplete=off size=4 maxlength=12 value=\"\" onkeypress=\"javascript:return cF2(event,\'ch\'," + c1 + ",\'b_yy\');\" onkeydown=\"if(event.keyCode == 13)javascript:evSD('" + t1 + "b');return oKD1(event,'" + t1 + "b_yy','" + t1 + "b_dd');\" onblur=\"javascript:evSD('" + t1 + "b')\"\/>"
        var d_d ="\
<input id=" + t1 + "d_mm name=" + t1 + "d_mm class=m autocomplete=off size=1 maxlength=2 value=\"\" list=\"dlmonth\" onkeypress=\"javascript:return oKPmm(event,'" + t1 + "d','_mm','_dd');\"\/>\
<input id=" + t1 + "d_dd name=" + t1 + "d_dd class=d autocomplete=off size=1 maxlength=2 value=\"\" onkeypress=\"javascript:return oKPdd(event,'" + t1 + "d','_dd','_yy');\" onkeydown=\"javascript:return oKD1(event,'" + t1 + "d_dd','" + t1 + "d_mm');\"\/>\
<input id=" + t1 + "d_yy name=" + t1 + "d_yyyy class=ys autocomplete=off size=4 maxlength=12 value=\"\" onkeydown=\"if(event.keyCode == 13)javascript:evSD('" + t1 + "d');return oKD1(event,'" + t1 + "d_yy','" + t1 + "d_dd');\" onkeypress=\"javascript:return cF2(event,\'ch\'," + c1 + ",\'d_yy\');\" onblur=\"javascript:evSD('" + t1 + "d')\"\/>"
      }
      if (dmy == "yyyymmdd")
      {
        var d_b ="\
<input id=" + t1 + "b_yy name=" + t1 + "b_yyyy class=ys autocomplete=off size=4 maxlength=12 value=\"\" onkeypress=\"javascript:return cF2(event,\'ch\'," + c1 + ",\'b_yy\');\" onkeydown=\"if(event.keyCode == 13)javascript:evSD('" + t1 + "b')\" onblur=\"javascript:evSD('" + t1 + "b')\"\/>\
<input id=" + t1 + "b_mm name=" + t1 + "b_mm class=m autocomplete=off size=1 maxlength=2 value=\"\" list=\"dlmonth\" onkeypress=\"javascript:return oKPmm(event,'" + t1 + "b','_mm','_dd');\" onkeydown=\"javascript:return oKD1(event,'" + t1 + "b_mm','" + t1 + "b_yy');\"\/>\
<input id=" + t1 + "b_dd name=" + t1 + "b_dd class=d autocomplete=off size=1 maxlength=2 value=\"\" onkeypress=\"javascript:return oKPdd(event,'" + t1 + "b','_dd','_pl');\" onkeydown=\"javascript:return oKD1(event,'" + t1 + "b_dd','" + t1 + "b_mm');\"\/>"
        var d_d ="\
<input id=" + t1 + "d_yy name=" + t1 + "d_yyyy class=ys autocomplete=off size=4 maxlength=12 value=\"\" onkeypress=\"javascript:return cF2(event,\'ch\'," + c1 + ",\'d_yy\');\" onkeydown=\"if(event.keyCode == 13)javascript:evSD('" + t1 + "d')\" onblur=\"javascript:evSD('" + t1 + "d')\"\/>\
<input id=" + t1 + "d_mm name=" + t1 + "d_mm class=m autocomplete=off size=1 maxlength=2 value=\"\" list=\"dlmonth\" onkeypress=\"javascript:return oKPmm(event,'" + t1 + "d','_mm','_dd');\" onkeydown=\"javascript:return oKD1(event,'" + t1 + "d_mm','" + t1 + "d_yy');\"\/>\
<input id=" + t1 + "d_dd name=" + t1 + "d_dd class=d autocomplete=off size=1 maxlength=2 value=\"\"\ onkeypress=\"javascript:return oKPdd(event,'" + t1 + "d','_dd','_pl');\" onkeydown=\"javascript:return oKD1(event,'" + t1 + "d_dd','" + t1 + "d_mm');\"/>"
      }
      document.getElementById("new_child").outerHTML = '\
      <tr id="' + t1 + '">\n\
        <td>\n\
          <input type="hidden" id="' + t1 + '_sex" name="' + t1 + '_sex" value="N">\n\
          <input type="hidden" id="' + t1 + '_p" name="' + t1 + '_p" value="create">\n\
          <input id="' + t1 + '_occ" name="' + t1 + '_occ" class="occ2" autocomplete="off" size="5" maxlength="8" placeholder="N" value="" onkeypress="javascript:return oKP2(event,\'ch\',' + c1 + ',\'_occ\',\'\')" onblur="oB2(\'' + t1 + '\')">\n\
          <div id="' + t1 + '_jq1"> <\/div>\n\
        <\/td>\n\
        <td><input id="' + t1 + '_fn" name="' + t1 + '_fn" size="30" maxlength="200" value="" onkeypress="javascript:return cF2(event,\'ch\',' + c1 + ',\'_fn\');" onkeydown="if(event.keyCode == 13)tUC1(this)" onblur="tUC1(this);jq1a(\'' + t1 + '\')" ' + g_fn + '><br>\
            <input type="' + v2 + '" id="' + t1 + '_sn" name="' + t1 + '_sn" class="ar" size="30" maxlength="200" value="' + v3 + '" placeholder="' + v1 + '" onkeypress="javascript:return cF2(event,\'ch\',' + c1 + ',\'_sn\');" onblur="tUC(this);jq1a(\'' + t1 + '\')" onkeydown="if(event.keyCode == 13)tUC(this)" ' + g_sn + '><\/td>\n\
        <td class="jq2"><div id="' + t1 + '_jq2"> <\/div><div id="' + t1 + '_jq3"> <\/div>\n\
            <span id="dp' + t1 + '" class="vis">\n\
              <span class="dmyt">' + d_b + '<input id="' + t1 + 'b_pl" name="' + t1 + 'b_pl" class="pl" size="44" maxlength="200" value="" onblur="fillPlaceFam(this)" ' + g_place + '><\/span>\n\
              <span class="dmyt">' + d_d + '<input id="' + t1 + 'd_pl" name="' + t1 + 'd_pl" class="pl" size="44" maxlength="200" value="" onblur="fillPlaceFam(this)" ' + g_place + '><\/span>\n\
            <\/span><\/td>\n\
        <td class="jq4"><div id="' + t1 + '_jq4"> <\/div><input id="' + t1 + '_occupation" name="' + t1 + '_occupation" class="occu vis" size="40" maxlength="200" value="" ' + g_occu + '><\/td>\n\
      <\/tr>\n\
      <tr id="new_child"><\/tr>';
      if(z2 == 0)
      {
        for(var i = c1; i > c0; i--)invertChild(i);
        sIV(t0 + "_occ");
      }
      else sIV(t1 + "_occ");
    }
    else sIV("ch1_occ");
  }
  function invertChild(cnt)
  {
    if (cnt > 0)
    {
      var t0 = "ch" + (cnt - 1);
      var t1 = "ch" + cnt;
      var a = new Array();
      
      a[0] = new Array();
      a[0]["e10"]= t0 + "_fn";
      a[0]["e11"]= t0 + "_sn";
      a[0]["e12"]= t0 + "_occ";
      a[0]["e13"]= t0 + "b_dd";
      a[0]["e14"]= t0 + "b_mm";
      a[0]["e15"]= t0 + "b_yy";
      a[0]["e16"]= t0 + "b_pl";
      a[0]["e17"]= t0 + "d_dd";
      a[0]["e18"]= t0 + "d_mm";
      a[0]["e19"]= t0 + "d_yy";
      a[0]["e20"]= t0 + "d_pl";
      a[0]["e21"]= t0 + "_occupation";
      a[0]["e22"]= t0 + "_sex";
      a[0]["e23"]= t0 + "_p";
      a[0]["e24"]= "dp" + t0;
      a[0]["e25"]= t0 + "_occupation";
      a[0]["e26"]= t0 + "_occ";
      a[0]["e27"]= t0 + "_jq1";
      a[0]["e28"]= t0 + "_jq2";
      a[0]["e29"]= t0 + "_jq3";
      a[0]["e30"]= t0 + "_jq4";

      a[1] = new Array();
      a[1]["e10"]= t1 + "_fn";
      a[1]["e11"]= t1 + "_sn";
      a[1]["e12"]= t1 + "_occ";
      a[1]["e13"]= t1 + "b_dd";
      a[1]["e14"]= t1 + "b_mm";
      a[1]["e15"]= t1 + "b_yy";
      a[1]["e16"]= t1 + "b_pl";
      a[1]["e17"]= t1 + "d_dd";
      a[1]["e18"]= t1 + "d_mm";
      a[1]["e19"]= t1 + "d_yy";
      a[1]["e20"]= t1 + "d_pl";
      a[1]["e21"]= t1 + "_occupation";
      a[1]["e22"]= t1 + "_sex";
      a[1]["e23"]= t1 + "_p";
      a[1]["e24"]= "dp" + t1;
      a[1]["e25"]= t1 + "_occupation";
      a[1]["e26"]= t1 + "_occ";
      a[1]["e27"]= t1 + "_jq1";
      a[1]["e28"]= t1 + "_jq2";
      a[1]["e29"]= t1 + "_jq3";
      a[1]["e30"]= t1 + "_jq4";

      for (var i = 10; i <= 23; i++)
      {
        var v0 = document.getElementById(a[0]["e" + i]).value; 
        var v1 = document.getElementById(a[1]["e" + i]).value;
        document.getElementById(a[0]["e" + i]).value = v1;
        document.getElementById(a[1]["e" + i]).value = v0;
      }
      for (var i = 24; i <= 26; i++)
      {
        var v0 = document.getElementById(a[0]["e" + i]).className;
        var v1 = document.getElementById(a[1]["e" + i]).className;
        document.getElementById(a[0]["e" + i]).className = v1;
        document.getElementById(a[1]["e" + i]).className = v0;
      }
      var v0 = document.getElementById(a[0]["e26"]).getAttribute("placeholder","false");
      var v1 = document.getElementById(a[1]["e26"]).getAttribute("placeholder","false");
      document.getElementById(a[0]["e26"]).setAttribute("placeholder",v1,"false");
      document.getElementById(a[1]["e26"]).setAttribute("placeholder",v0,"false");
      var v0 = document.getElementById(a[0]["e27"]).title;
      var v1 = document.getElementById(a[1]["e27"]).title;
      document.getElementById(a[0]["e27"]).title = v1;
      document.getElementById(a[1]["e27"]).title = v0;
      for (var i = 27; i <= 30; i++)
      {
        var e0 = document.getElementById(a[0]["e" + i]);
        var v0 = e0.firstChild ? e0.firstChild.data : "";
        var e1 = document.getElementById(a[1]["e" + i]);
        var v1 = e1.firstChild ? e1.firstChild.data : "";
        document.getElementById(a[0]["e" + i]).innerHTML = v1;
        document.getElementById(a[1]["e" + i]).innerHTML = v0;
      }
    }
  }
  function addWitness(z1,z2,z3)
  {
    var t3 = "e" + z3 + "_witn";
    var c0 = z1;
    var c1 = itemMaxCnt(t3,c0);
    var t0 = t3 + c0;
    var t1 = t3 + c1;
    document.getElementById("new_" + t3).outerHTML = '\
    <dd id="' + t1 + '"\n\
    ><input type="hidden" id="' + t1 + '_kind" name="' + t1 + '_kind" value=""\n\
    ><input type="hidden" id="' + t1 + '_sex" name="' + t1 + '_sex" value="N"\n\
    ><input type="hidden" id="' + t1 + '_p" name="' + t1 + '_p" value="create"\n\
    ><input id="' + t1 + '_occ" name="' + t1 + '_occ" class="occ2" autocomplete="off" size="5" maxlength="8" value="" placeholder="N" onkeypress="javascript:return oKP2(event,\'witn\',' + c1 + ',\'_occ\',' + z3 + ')" onblur="oB2(\'' + t1 + '\')"\n\
    ><input id="' + t1 + '_fn" name="' + t1 + '_fn" class="fn ar" size="30" maxlength="200" value="" onblur="tUC1(this)" onkeydown="if(event.keyCode == 13)tUC1(this)" ' + g_fn + '\n\
    ><input id="' + t1 + '_sn" name="' + t1 + '_sn" class="sn" size="30" maxlength="200" value="" onblur="tUC(this);jq1(\''+ t1 +'\',\'\')" onkeydown="if(event.keyCode == 13)tUC(this)" ' + g_sn + '><span id="'+ t1 +'_jq1"> <\/span><\/dd>\n\
    <dd id="new_' + t3 + '"><\/dd>';
    if(z2 == 0)
    {
      for(var i = c1; i > c0; i--){invertWitness(i,z3,0);}
      sIV(t0 + "_occ");
    }
    else sIV(t1 + "_occ");
  }
  function invertWitness(cnt,z2,z3)
  {
    if (cnt > 0)
    {
      var t0 = (z3 > 0) ? "e" + (z3 - 1) + "_witn" + cnt: "e" + z2 + "_witn" + (cnt - 1);
      var t1 = (z3 > 0) ? "e" + z3 + "_witn" + cnt: "e" + z2 + "_witn" + cnt;
      var a = new Array();
      
      a[0] = new Array();
      a[0]["e10"]= t0 + "_fn";
      a[0]["e11"]= t0 + "_sn";
      a[0]["e12"]= t0 + "_occ";
      a[0]["e13"]= t0 + "_sex";
      a[0]["e14"]= t0 + "_p";
      a[0]["e15"]= t0 + "_kind";
      a[0]["e16"]= t0 + "_occ";
      a[0]["e17"]= t0 + "_jq1";
      
      a[1] = new Array();
      a[1]["e10"]= t1 + "_fn";
      a[1]["e11"]= t1 + "_sn";
      a[1]["e12"]= t1 + "_occ";
      a[1]["e13"]= t1 + "_sex";
      a[1]["e14"]= t1 + "_p";
      a[1]["e15"]= t1 + "_kind";
      a[1]["e16"]= t1 + "_occ";
      a[1]["e17"]= t1 + "_jq1";

      for (var i = 10; i <= 15; i++)
      {
        var v0 = document.getElementById(a[0]["e" + i]).value;
        var v1 = document.getElementById(a[1]["e" + i]).value;
        document.getElementById(a[0]["e" + i]).value = v1;
        document.getElementById(a[1]["e" + i]).value = v0;
      }
      var v0 = document.getElementById(a[0]["e16"]).className;
      var v1 = document.getElementById(a[1]["e16"]).className;
      document.getElementById(a[0]["e16"]).className = v1;
      document.getElementById(a[1]["e16"]).className = v0;
      var v0 = document.getElementById(a[0]["e16"]).getAttribute("placeholder","false");
      var v1 = document.getElementById(a[1]["e16"]).getAttribute("placeholder","false");
      document.getElementById(a[0]["e16"]).setAttribute("placeholder",v1,"false");
      document.getElementById(a[1]["e16"]).setAttribute("placeholder",v0,"false");
      var v0 = document.getElementById(a[0]["e17"]).title;
      var v1 = document.getElementById(a[1]["e17"]).title;
      document.getElementById(a[0]["e17"]).title = v1;
      document.getElementById(a[1]["e17"]).title = v0;
      for (var i = 17; i <= 17; i++)
      {
        var e0 = document.getElementById(a[0]["e" + i]);
        var v0 = e0.firstChild ? e0.firstChild.data : "";
        var e1 = document.getElementById(a[1]["e" + i]);
        var v1 = e1.firstChild ? e1.firstChild.data : "";
        document.getElementById(a[0]["e" + i]).innerHTML = v1;
        document.getElementById(a[1]["e" + i]).innerHTML = v0;
      }
    }
  }
  function addPvar(z1,z2)
  {
    var c0 = z1;
    var c1 = itemMaxCnt('p',c0);
    var t0 = "p" + c0;
    var t1 = "p" + c1;  
    document.getElementById("new_pvar").outerHTML = '\
    <tr id="' + t1 + '"\n\
    ><td class="b1"><a tabindex="10000" href="javascript:addPvar(' + c1 + ',\'0\');">+<\/a\n\
    ><\/td><td><input id="' + t1 + '_occ" name="oc' + c1 + '" class="form-control occ3" autocomplete="off" size="5" maxlength="8" value="" onkeypress="javascript:return oKP2(event,\'p\',' + c1 + ',\'_occ\',\'\')"\n\
    ><td class="b1"><a tabindex="10000" href="javascript:delS(\'p\',' + c1 + ',\'_occ\',\'\');">x<\/a\n\
    ><\/td><td><input id="' + t1 + '_fn" name="p' + c1 + '" class="form-control ar" size="30" maxlength="200" value="" onblur="tUC1(this)"\n\
    ><\/td><td><input id="' + t1 + '_sn" name="n' + c1 + '" class="form-control" size="30" maxlength="200" value="" onblur="tUC(this);jq1(\''+ t1 +'\',\'\')"\n\
    ><td class="b1"><a tabindex="10000" href="javascript:invertS(\'p\',' + c1 + ',\'_occ\',0,1,\'\');">&uarr;<\/a\n\
    ><\/td><td class="b1"><span id="'+ t1 +'_jq1"> <\/span\n\
    ><td class="b1"><a tabindex="10000" href="javascript:invertS(\'p\',' + c1 + ',\'_occ\',1,1,\'\');">&darr;<\/a\n\
    ><\/td><td><input id="' + t1 + '_t" name="t' + c1 + '" class="form-control" size="30" maxlength="200" value=""\n\
    ><input type="hidden" id="' + t1 + '_i" name="i' + c1 + '" size="5" value=""\n\
    ><\/td><\/tr><tr id="new_pvar"><\/tr>';
    if(z2 == 0)
    {
      for(var i = c1; i > c0; i--){invertPvar(i);}
      sIV(t0 + "_occ");
    }
    else sIV(t1 + "_occ");
  }
  function invertPvar(cnt)
  {
    if (cnt > 0)
    {
      var t0 = "p" + (cnt - 1);
      var t1 = "p" + cnt;
      var a = new Array();
      a[0] = new Array();
      a[0]["e10"]= t0 + "_fn";
      a[0]["e11"]= t0 + "_sn";
      a[0]["e12"]= t0 + "_occ";
      a[0]["e13"]= t0 + "_t";
      a[0]["e14"]= t0 + "_i";
      a[0]["e20"]= t0 + "_jq1";

      a[1] = new Array();
      a[1]["e10"]= t1 + "_fn";
      a[1]["e11"]= t1 + "_sn";
      a[1]["e12"]= t1 + "_occ";
      a[1]["e13"]= t1 + "_t";
      a[1]["e14"]= t1 + "_i";
      a[1]["e20"]= t1 + "_jq1";

      for (var i = 10; i <= 14; i++)
      {
        var v0 = document.getElementById(a[0]["e" + i]).value; 
        var v1 = document.getElementById(a[1]["e" + i]).value;
        document.getElementById(a[0]["e" + i]).value = v1;
        document.getElementById(a[1]["e" + i]).value = v0;
      }
      for (var i = 20; i <= 20; i++)
      {
        var e0 = document.getElementById(a[0]["e" + i]);
        var v0 = e0.firstChild ? e0.firstChild.data : "";
        var e1 = document.getElementById(a[1]["e" + i]);
        var v1 = e1.firstChild ? e1.firstChild.data : "";
        document.getElementById(a[0]["e" + i]).innerHTML = v1;
        document.getElementById(a[1]["e" + i]).innerHTML = v0;
      }
    }
  } 
  function cF1(event,id)
  {
    var key = event.keyCode ? event.keyCode : event.which ? event.which : event.charCode;
    var charKey = String.fromCharCode(key);
    if (charKey == "*" || charKey == "_")
    {
      document.getElementById(id).focus();
      return false;
    }
  }
  function oKPdd(event,z2,z3,z4)
  {
    var key = event.keyCode ? event.keyCode : event.which ? event.which : event.charCode;
    var charKey = String.fromCharCode(key);
    var e1 = document.getElementById(z2 + z3);
    var e2 = document.getElementById(z2 + z4);
    var e3 = document.getElementById(z2 + "_mm");
    var e4 = document.getElementById(z2 + "_yy");
    var v = e1.value;
    var v3 = e3.value;
    var v4 = e4.value;
    var l = v.length;
    if (charKey >= 0 && charKey <= 9)
    {
      var s = v + charKey;
      var ev = isFinite(s) ? eval(s): 0;
      if (l == 1)
      {
        if (ev > 0 && ev < 32)
        {
          if (ev == 31)
          {
            var r = /02|04|06|09|11/;
            var t = r.test(v3);
            if (t == true)
            {
              e1.value = "";
            }
            else
            {
              e1.value = s; e2.select();
            }
          }
          else
          {
            if (v3 == "02" && ev > 28)
            {
              if (ev > 29)
              {
                e1.value = "";
              }
              else
              { 
                if ((v4 != "") && ((v4 < 1582) || ((v4 % 4 != 0) || ((v4 % 100 == 0) && (v4 % 400 != 0)))))
                {
                  e1.value = "";
                }
                else
                {
                  e1.value = s; e2.select();
                }
              }
            }
            else
            {
              e1.value = s; e2.select();
            }
          }
        }
        else
        {
          e1.value = "";
        }
        return false;
      }
    }
  }
  function oKPmm(event,z2,z3,z4)
  {
    var key = event.keyCode ? event.keyCode : event.which ? event.which : event.charCode;
    var charKey = String.fromCharCode(key);
    var e1 = document.getElementById(z2 + z3);
    var e2 = document.getElementById(z2 + z4);
    var e3 = document.getElementById(z2 + "_dd");
    var e4 = document.getElementById(z2 + "_yy");
    var v = e1.value;
    var v3 = e3.value;
    var v4 = e4.value;
    var l = v.length;
    if (l == 1)
    {
      var s = v + charKey;
      if (isNaN(s) == true)
      {
        var s1 = s.toUpperCase();
        var r1 = /VD|BR|FM|NI|PL|VT|GE|FL|PR|ME|TH|FT|JC/;
        var t1 = r1.test(s1);
        if (t1 == true)
        {
          e1.value = s1; e2.select();
        }
        else
        {
          e1.value = "";
        }
      }
      else
      {
        var ev = eval(s);
        if (ev > 0 && ev < 13)
        {
          if (v3 == 31 && ev != 12)
          {
            var r = /2|4|6|9|11/;
            var t = r.test(ev);
            if (t == true)
            {
              e1.value = "";
            }
            else
            {
              e1.value = s; e2.select();
            }
          }
          else
          {
            if (ev == 2 && v3 > 28)
            {
              if (v3 > 29)
              {
                e1.value = "";
              }
              else
              {  
                if ((v4 != "") && ((v4 < 1582) || ((v4 % 4 != 0) || ((v4 % 100 == 0) && (v4 % 400 != 0)))))
                {
                  e1.value = "";
                }
                else
                {
                  e1.value = s; e2.select();
                }
              }
            }
            else
            {
              e1.value = s; e2.select();
            }
          }
        }
        else
        {
          e1.value = "";
        }
      }
      return false;
    }
  }
  function oKD1(event,z2,z3)
  {
    var key = event.keyCode ? event.keyCode : event.which ? event.which : event.charCode;
    var e1 = document.getElementById(z2);
    var e2 = document.getElementById(z3);
    var v = e1.value;
    var l = v.length;
    if (l == 0 && key == "8")
    {
      e2.select();
      return false;
    }
  }
  function cF2(event,z2,z3,z4)
  {
    var key = event.keyCode ? event.keyCode : event.which ? event.which : event.charCode;
    var charKey = String.fromCharCode(key);
    if (charKey == "*" || charKey == "_")
    {
      var n = z3 + 1;
      var t = z2 + z3;
      var id = z2 + z3 + z4;
      var v1 = document.getElementById(t + "_p").value;
      var v2 = document.getElementById("pa2_fn").value;
      var s = v1 + z4;
      if (t == "pa1")
      {
        switch(s)
        {
          case "create_fn": id = "pa1_sn"; break;
          case "create_sn": id = "pa1b_yy"; break;
          case "createb_yy": id = "pa1d_yy"; break;
          default: id = "pa2_fn"; break;
        }
      }
      else
      {
        if (t == "pa2")
        {
          switch(s)
          {
            case "create_fn": id = "pa2_sn"; break;
            case "create_sn": id = "pa2b_yy"; break;
            case "createb_yy": id = "pa2d_yy"; break;
            default: id = "ach"; break;
          }
        }
        else
        {
          switch(s)
          {
            case "create_fn": id = t + "b_yy"; break;
            case "create_sn": id = t + "b_yy"; break;
            case "createb_yy": id = t + "d_yy"; break;
            default: id = "ach"; break;
          }
        }
      }
      document.getElementById(id).focus();
      document.getElementById(id).click();
      return false;
    }
  }
  function evD(z1)
  {
    var e = document.getElementById(z1 + "_yy");
    var e1 = document.getElementById(z1 + "_yyyy");
    var e2 = document.getElementById(z1 + "_mm");
    var e3 = document.getElementById(z1 + "_dd");
    var e4 = document.getElementById(z1 + "_oryear");
    var e5 = document.getElementById(z1 + "_ormonth");
    var e6 = document.getElementById(z1 + "_orday");
    var e7 = document.getElementById(z1 + "_prec");
    var e8 = document.getElementById(z1 + "_text");
    var dmy = document.getElementById("dmy").firstChild.data;
    var v = e.value;
    var v3 = e3.value;
    var v2 = e2.value;
    var r1 = /(^-\d*$|^\d*$)/;
    var t1 = r1.test(v);
    if ((v3 == "29" && v2 == "02") && ((v < 1582) || ((v % 4 != 0) || ((v % 100 == 0) && (v % 400 != 0)))))
    {
      e.value = ""; return false;
    }
    if(v == "-" || v == "+"){e1.value = ""; e2.value = ""; e3.value = ""; e4.value = ""; e7.value = ""; e8.value = v;}
    else
    {
      if(t1 == true) {e1.value = RegExp.$1; e7.value = "sure"; e8.value = "";}
      else
      {
        r1 = /(^\/)(-\d*$|\d*$)/; t1 = r1.test(v);
        if(t1 == true) {e1.value = RegExp.$2; e7.value = "before"; e8.value = "";}
        else
        {
          r1 = /(^-\d*|^\d*)(\/$)/; t1 = r1.test(v);
          if(t1 == true) {e1.value = RegExp.$1; e7.value = "after"; e8.value = "";}
          else
          {
            r1 = /(^|^\?|^\.|^\.\.)(-\d*|\d*)($|\?$|\.$|\.\.$)/; t1 = r1.test(v);
            if(t1 == true){e.value = "?" + RegExp.$2; e1.value = RegExp.$2; e7.value = "maybe"; e8.value = "";}
            else
            {
              r1 = /(^|^\/|^\/\/)(-\d*|\d*)($|\/$|\/\/$)/; t1 = r1.test(v);
              if(t1 == true){e.value = "/" + RegExp.$2 + "/"; e1.value = RegExp.$2; e7.value = "about"; e8.value = "";}
              else
              {
                r1 = /(^-\d*|^\d*)(\+|-)(\d*$)/; t1 = r1.test(v);
                if(t1 == true)
                {
                  var ev = eval(v);
                  e.value = "/" + ev + "/"; e1.value = ev; e7.value = "about"; e8.value = "";}
                else
                { /* or/int year */
                  r1 = /(^-\d*|^\d*)(\/|\.|\.\.)(-\d*$|\d*$)/; t1 = r1.test(v);
                  if(t1 == true)
                    {
                      e1.value = RegExp.$1; e4.value = RegExp.$3; e5.value = ""; e6.value = ""; e8.value = "";
                      if(RegExp.$2 == "/") e7.value = "oryear"; else{e7.value = "yearint";}
                    }
                  else
                  { /* or/int month/year */
                    r1 = /(^-\d*|^\d*)(\/|\.|\.\.)(\w*)(\W)(\w*$)/; t1 = r1.test(v);
                    if(t1 == true)
                    {
                      e1.value = RegExp.$1; e6.value = ""; e8.value = "";
                      if(dmy != "yyyymmdd"){e4.value = RegExp.$5; e5.value = RegExp.$3;}
                      else{e4.value = RegExp.$3; e5.value = RegExp.$5;}
                      if(RegExp.$2 == "/") e7.value = "oryear"; else{e7.value = "yearint";}
                    }
                    else
                    { /* or/int day/month/year */
                      r1 = /(^-\d*|^\d*)(\/|\.|\.\.)(\w*)(\W)(\w*)(\W)(\w*$)/; t1 = r1.test(v);
                      if(t1 == true)
                      {
                        e1.value = RegExp.$1; e8.value = "";
                        if(dmy == "ddmmyyyy"){e4.value = RegExp.$7; e5.value = RegExp.$5; e6.value = RegExp.$3;}
                        if(dmy == "mmddyyyy"){e4.value = RegExp.$7; e5.value = RegExp.$3; e6.value = RegExp.$5;}
                        if(dmy == "yyyymmdd"){e4.value = RegExp.$3; e5.value = RegExp.$5; e6.value = RegExp.$7;}
                        if(RegExp.$2 == "/") e7.value = "oryear"; else{e7.value = "yearint";}
                      }
                      else{e1.value = ""; e7.value = ""; e4.value = ""; e8.value = v; e3.value = ""; e2.value = ""}
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  function evSD(z1)
  {
    var e = document.getElementById(z1 + "_yy");
    var e1 = document.getElementById(z1 + "_dd");
    var e2 = document.getElementById(z1 + "_mm");
    var v = e.value;
    var v1 = e1.value;
    var v2 = e2.value;
    var r1 = /(^-\d*$|^\d*$)/;
    var t1 = r1.test(v);
    if ((v1 == "29" && v2 == "02") && ((v < 1582) || ((v % 4 != 0) || ((v % 100 == 0) && (v % 400 != 0)))))
    { 
      e.value = ""; return false;
    }
    if(v == "-" || v == "+"){}
    else
    {
      if(t1 == true) {e.value = RegExp.$1;}
      else
      {
        r1 = /(^\/|^<)(-\d*$|\d*$)/; t1 = r1.test(v);
        if(t1 == true) {e.value = "/" + RegExp.$2}
        else
        {
          r1 = /(^>)(-\d*$|\d*$)/; t1 = r1.test(v);
          if(t1 == true) {e.value = RegExp.$2 + "/"}
          else
          {
            r1 = /(^-\d*|^\d*)(\/$)/; t1 = r1.test(v);
            if(t1 == true) {e.value = RegExp.$1 + "/"}
            else
            {
              r1 = /(^|^\?|^\.|^\.\.)(-\d*|\d*)($|\?$|\.$|\.\.$)/; t1 = r1.test(v);
              if(t1 == true) {e.value = "?" + RegExp.$2}
              else
              {
                r1 = /(^|^\/|^\/\/)(-\d*|\d*)($|\/$|\/\/$)/; t1 = r1.test(v);
                if(t1 == true) {e.value = "/" + RegExp.$2 + "/"}
                else
                {
                  r1 = /(^-\d*|^\d*)(\+|-)(\d*$)/; t1 = r1.test(v);
                  if(t1 == true) {var ev = eval(v); e.value = "/" + ev + "/"}
                  else e.value = "";
                }
              }
            }
          }
        }
      }
    }
  }
  function visHid(z1,z2)
  {
    var e1 = document.getElementById(z1 + "_hid"); 
    var e2 = document.getElementById(z1 + "_vis");
    var s = e1.style.display;
    var i = 1;
    
    if (s == "none")
    {
      e1.style.display = "inline"; e2.style.display = "none";
      while (document.getElementById(z1 + i + z2))
      {document.getElementById(z1 + i + z2).type = "text";i++}
    }
    else
    {
      e1.style.display = "none"; e2.style.display = "inline";
      while (document.getElementById(z1 + i + z2))
      {document.getElementById(z1 + i + z2).type = "hidden";i++}
    }
  }
  function addS(z1,z2,z3,z4)
  {
    switch(z1)
    {
      case "ch": addChild(z2,z3); break;
      case "e": addEvent(z2,z3); break;
      case "p": addPvar(z2,z3); break;
      case "r": addRelation(z2,z3); break;
      case "t": addTitle(z2,z3); break;
      case "witn": addWitness(z2,z3,z4); break;
    }
  }
  function invertS(z1,z2,z3,z4,z5,z6)
  {
    var t1 = (z1 == "witn") ? "e" + z6 + "_witn": z1;
    var n = Number(z2);
    var i1 = (z4 == 0) ? n: n + 1;
    var i2 = (z4 == 0) ? n - 1: n + 1;
    var e1 = (document.getElementById(t1 + i1 + z3)) ? document.getElementById(t1 + i1 + z3) : "";
    var e2 = (document.getElementById(t1 + i2 + z3)) ? document.getElementById(t1 + i2 + z3) : "";
    if (e1 != "" && e2 != "")
    {
      if (z5 == 1)
      {
        switch(z1)
        {
          case "ch": invertChild(i1); break;
          case "e": invertEvent(i1); break;
          case "p": invertPvar(i1); break;
          case "r": invertRelation(i1); break;
          case "t": invertTitle(i1); break;
          case "witn": invertWitness(i1,z6,0); break;
        }
      }
      document.getElementById(t1 + i2 + z3).focus();
    }
  }
  function delS(z1,z2,z3,z4)
  {
    var t1 = z1 == "witn" ? "e" + z4 + "_witn": z1;
    if (z1 != "r")
    {
      var i = z2;
      while (document.getElementById(t1 + i + z3))
      {
        var e = document.getElementById(t1 + i + z3);
        invertS(z1,i,z3,1,1,z4);
        i++;
      }
      var j = i - 1;
      if (j == 1)
      {
        addS(z1,z2,0,z4);
        j++;
      }
      var e1 = (document.getElementById(t1 + j)) ? document.getElementById(t1 + j): "";
      if (e1 != "") e1.outerHTML = '';
      var k = j > 1 ? j - 1: 1;
      var f = z2 == j ? document.getElementById(t1 + k + "_occ"):document.getElementById(t1 + z2 + "_occ");
      f.focus();
    }
  }
  function oB1()
  {
    var e1 = document.getElementById("sex1");
    var e2 = document.getElementById("sex");
    var v = e1.value;
    switch(v)
    {
      case "M": e1.className='occ0'; e2.value = "M"; e1.setAttribute("placeholder","M","false"); break;
      case "F": e1.className='occ1'; e2.value = "F"; e1.setAttribute("placeholder","F","false"); break;
      case "N": e1.className='occ2'; e2.value = "N"; e1.setAttribute("placeholder","N","false"); break;
    }
    e1.value="";
  }
  function oB2(z1)
  {
    var e1 = document.getElementById(z1 + "_sex");
    var e2 = document.getElementById(z1 + "_p");
    var e3 = document.getElementById(z1 + "_occ");
    var e4 = (document.getElementById("dp" + z1)) ? document.getElementById("dp" + z1) : "";
    var e5 = (document.getElementById(z1 + "_occupation")) ? document.getElementById(z1 + "_occupation") : "";
    var v = e3.value;
    var r = /(^\d*$)/;
    var t = r.test(v);
    if(v != "" && t == true)
    {
      e3.className='occ'; e1.value = ""; e2.value = "link";
      if(e4 != ""){e4.className='hid'; e5.className='occu hid';}
    }
    else
    {
      switch(v)
      {
        case "M": e3.className='occ0'; e1.value = "M"; e3.setAttribute("placeholder","M","false"); break;
        case "F": e3.className='occ1'; e1.value = "F"; e3.setAttribute("placeholder","F","false"); break;
        case "N": e3.className='occ2'; e1.value = "N"; e3.setAttribute("placeholder","N","false"); break;
      }
      e2.value = "create"; e3.value="";
      if(e4 != ""){e4.className='vis'; e5.className='occu vis';}
    }
  }
  function oB3(z1)
  {
    var e1 = document.getElementById(z1 + "_p");
    var e2 = document.getElementById(z1 + "_occ");
    var v = e2.value;
    var v1 = e2.getAttribute("placeholder","false");
    var r = /(^\d*$)/;
    var t = r.test(v);
    if(v != "" && t == true)e1.value = "link"; else {e1.value = "create"; e2.value = ""; e2.setAttribute("placeholder",v1,"false");}
  }
  function oB4(z1)
  {
    var e1 = document.getElementById(z1);
    var e2 = document.getElementById(z1 + "_p");
    var e3 = document.getElementById(z1 + "_occ");
    var e4 = document.getElementById("dp" + z1);
    var e5 = document.getElementById(z1 + "_occupation");
    var v = e3.value;
    var v1 = e3.getAttribute("placeholder","false");
    var r = /(^\d*$)/;
    var t = r.test(v);
    if(v != "" && t == true)
    {
      e2.value = "link"; e4.className='hid'; e5.className='hid';
    }
    else
    {
      e2.value = "create"; e3.value = ""; e4.className='vis'; e5.className='vis';
    }
  }
  var Tim1;
  function oB5()
  {
    clearTimeout(Tim1);
  }
  function oF5(z1)
  {
    clearTimeout(Tim1);
    Tim1 = setTimeout(function(){z1.click();},500);
  }
  function oB6(z1)
  {
    var e1 = document.getElementById("e_name" + z1);
    var e2 = document.getElementById("e_name" + z1 + "_dl");
    var v1 = e1.value;
    var v2 = e2.value;
    var n1 = document.getElementById("dlevent").childNodes;
    for(var i = 0; i < n1.length; i++)
    {
      var n1v = n1[i].value;
      var n1i = n1[i].id;
      if (v2 == n1v || v2 == n1i)
      {
        e1.value = n1i;
        e2.value = n1v;
        i = 1000;
      }
    }
    if (i != 1001) e1.value = v2;
  }
  function oL1()
  {
    var i = 1;
    while(document.getElementById("e_name" + i))
    {
      oB6(i);
      i++;
    }
  }
  function oL2()
  {
    var v1 = document.getElementById("pa1_sn").value;
    var v2 = document.getElementById("ch1_sn").value;
    if (v2 != "" && v1 != v2) visHid('ch','_sn');
  }
  function fillPlaceFam(xx)
  {
    switch(xx.value)
    {
      case "1": xx.value = document.upd.pa1b_pl.value; break;
      case "2": xx.value = document.upd.pa1d_pl.value; break;
      case "3": xx.value = document.upd.pa2b_pl.value; break;
      case "4": xx.value = document.upd.pa2d_pl.value; break;
      case "*": xx.value = document.upd.marr_place.value; break;
      case "0": xx.value = document.upd.ch1b_pl.value; break;
      case "+": xx.value = document.upd.ch1d_pl.value; break;
    }
  }
  function fillPlaceInd(xx)
  {
    switch(xx.value)
    {
      case "2": xx.value = document.upd.birth_place.value;break;
      case "3": xx.value = document.upd.death_place.value;break;
    }
  }
  function dsp0(z1)
  {
    document.getElementById(z1 + "0").style.display = "none";
    document.getElementById(z1 + "1").style.display = "block";
    document.getElementById(z1 + "1a").focus();
  }
  function dsp1(z1)
  {
    document.getElementById(z1 + "0").style.display = "block";
    document.getElementById(z1 + "1").style.display = "none";
    document.getElementById(z1 + "0a").focus();
  }
  function sIV(id)
  {
    var e = document.getElementById(id);
    var s = e.nodeName == "INPUT" || e.nodeName == "TEXTAREA" ? document.getElementById(id): document.getElementById(e.nextSibling.id);
    s.scrollIntoView("true");
    window.scrollBy(0,-430);
    if(s.nodeName == "TEXTAREA")s.focus();else s.select();
  }
  function oS1()
  {
    var i = 1;
    while (document.getElementById("p" + i))
    {
      document.getElementById("p" + i + "_occ").setAttribute("disabled","");
      document.getElementById("p" + i + "_fn").setAttribute("disabled","");
      document.getElementById("p" + i + "_sn").setAttribute("disabled","");
      var e = document.getElementById("p" + i + "_t");
      if (e.value == "") e.setAttribute("disabled","");
      i++;
    }
    document.getElementById("upd").target = "blank";
    document.getElementById("upd").submit();
    clearTimeout(Tim1);
    Tim1 = setTimeout(function(){oU1();},500);
  }
  function oU1()
  {
    var i = 1;
    while (document.getElementById("p" + i))
    {
      document.getElementById("p" + i + "_occ").removeAttribute("disabled","");
      document.getElementById("p" + i + "_fn").removeAttribute("disabled","");
      document.getElementById("p" + i + "_sn").removeAttribute("disabled","");
      var e = document.getElementById("p" + i + "_t");
      if (e.value == "") e.removeAttribute("disabled","");
      i++;
    }
  }