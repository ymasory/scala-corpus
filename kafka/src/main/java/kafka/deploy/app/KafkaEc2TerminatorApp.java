/*
 * Copyright 2010 LinkedIn, Inc.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

package kafka.deploy.app;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import joptsimple.OptionSet;

import kafka.deploy.utils.Ec2Connection;
import kafka.deploy.utils.HostNamePair;
import kafka.deploy.utils.TypicaEc2Connection;

import com.xerox.amazonws.ec2.RegionInfo;

public class KafkaEc2TerminatorApp extends KafkaApp {

    public static void main(String[] args) throws Exception {
        new KafkaEc2TerminatorApp().run(args);
    }

    @Override
    protected String getScriptName() {
        return "kafka-ec2-terminator.sh";
    }

    @Override
    public void run(String[] args) throws Exception {
        parser.accepts("help", "Prints this help");
        parser.accepts("logging",
                       "Options are \"debug\", \"info\" (default), \"warn\", \"error\", or \"off\"")
              .withRequiredArg();
        parser.accepts("accessid", "Access ID (used instead of accessidfile)")
              .withRequiredArg();
        parser.accepts("accessidfile", "Access ID file (used instead of accessid)")
              .withRequiredArg();
        parser.accepts("secretkey", "Secret key (used instead of secretkeyfile)")
              .withRequiredArg();
        parser.accepts("secretkeyfile", "Secret key file (used instead of secretkey)")
              .withRequiredArg();
        parser.accepts("hostnames", "File containing host names")
              .withRequiredArg();
        parser.accepts("region",
                       "Region type; options are " + RegionInfo.REGIONURL_AP_SOUTHEAST + ", "
                               + RegionInfo.REGIONURL_EU_WEST + ", " + RegionInfo.REGIONURL_US_WEST
                               + ", " + RegionInfo.REGIONURL_US_EAST + " (default) ")
              .withRequiredArg();

        OptionSet options = parse(args);
        String accessId = getAccessId(options);
        String secretKey = getSecretKey(options);
        String regionUrl = getRegionUrl(options);

        Ec2Connection ec2Connection = new TypicaEc2Connection(accessId, secretKey, null, regionUrl);

        List<String> hostNames = new ArrayList<String>();
        File hostNamesFile = getInputFile(options, "hostnames");

        if(hostNamesFile == null)
            printUsage();

        if(hostNamesFile != null) {
            List<HostNamePair> hostNamePairs = getHostNamesPairsFromFile(hostNamesFile);

            for(HostNamePair hostNamePair: hostNamePairs)
                hostNames.add(hostNamePair.getExternalHostName());

            ec2Connection.deleteInstancesByHostName(hostNames);
        }
    }

}