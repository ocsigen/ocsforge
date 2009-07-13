<ocsigen>
  <!-- Example of ocsigen.conf for ocsimore. Adapt it to your configuration -->


  <server>

    <port>8080</port>
    <logdir>/home/raphael/src/ocsimore/var/log/ocsimore</logdir>
    <datadir>/home/raphael/src/ocsimore/var/lib/ocsimore</datadir>
    
    
    <charset>utf-8</charset>
    <commandpipe>/home/raphael/src/ocsimore/var/run/ocsimore_command</commandpipe>

    <debugmode/>

    <findlib path="/home/raphael/src/ocsimore/files"/>

    <extension findlib-package="ocsigen.ext.staticmod"/>
    <extension findlib-package="ocsigen.ext.ocsipersist-sqlite">
      <!-- If you want to specify the SQLITE database file yourself:
      <database file="/PATH_TO/ocsigen/var/lib/ocsidb"/>
      -->
    </extension>

    <extension findlib-package="ocsigen.ext.eliom"/>
    <extension findlib-package="ocsigen.ext.eliom_duce"/>
    <extension findlib-package="ocsigen.ext.accesscontrol"/>
    <extension findlib-package="pgocaml"/>
    <extension findlib-package="ocsimore">
      <passwordfile name="/home/raphael/src/ocsimore/etc/ocsigen/ocsimorepassword" />
    </extension>
    <extension findlib-package="ocsimore.forum" />

    <!-- personal pages: -->
    <extension findlib-package="ocsigen.ext.userconf"/>
    <extension findlib-package="ocsigen.ext.extendconfiguration"/>
    <extension findlib-package="ocsigen.ext.redirectmod"/>
    <extension module="_build/ocsforge_svn.cma"/>    
    <!-- END personal pages -->			     


    <host defaulthostname="localhost">

      <static dir="/home/raphael/src/ocsforge/static" />
            

      <eliommodule findlib-package="ocsimore.users">
        <!-- The following arguments are optional: -->

        <notsecure/> <!-- accept login with http or https (DANGEROUS!)
                          If you do not want that option,
                          you need to configure HTTPS. -->

        <!-- For PAM authentication; Create your service file in /etc/pam.d -->
        <!--pam service="ocsimore" /-->
        <!-- Otherwise you can use NIS -->
        <nis />

        <!-- To create Ocsimore users: -->
        <basicusercreation registration_mail_from="Ocsimore" registration_mail_addr="ocsimore@somewhere.nowhere.com" registration_mail_subject="Ocsimore registration" groups="users"/>
      </eliommodule>

      <eliommodule findlib-package="ocsimore.ocsisite">
        <admin staticdir="/home/raphael/src/ocsforge/static" />
      </eliommodule>



      <eliommodule findlib-package="ocsimore.forum.site" />

      <!-- The following option has effect only the first time, and creates the
           specified wiki. Already created wikis are automatically reloaded -->

      <eliommodule module="_build/ocsforge.cma">
      </eliommodule>

      <eliommodule module="../ocsimore/_build/forum/ocsicreateforum.cmo">
        <name>task_forum</name>
      </eliommodule>

      <eliom/>

    </host>

    <!-- It is possible to define new Ocsimore groups from the config file: -->

    <!--extension findlib-package="ocsimore">
      <group name="intranet">
        <!-- Use one condition with accesscontrol's syntax: -->
        <or>
          <ip value="134.157.168.0/25" />
          <ip value="192.168.0.0/24" />
          <ip value="2001:660:3301:8061::/64" />
        </or>
      </group>
    </extension-->

  </server>

</ocsigen>
