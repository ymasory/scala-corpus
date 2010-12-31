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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import joptsimple.OptionException;
import joptsimple.OptionParser;
import joptsimple.OptionSet;
import kafka.deploy.utils.HostNamePair;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.xerox.amazonws.ec2.RegionInfo;

@SuppressWarnings("unchecked")
public abstract class KafkaApp {

    protected final OptionParser parser = new OptionParser();

    protected abstract String getScriptName();

    protected abstract void run(String[] args) throws Exception;

    protected void printUsage() {
        System.err.println("Usage: $KAFKA_HOME/contrib/deploy/bin/" + getScriptName());

        try {
            parser.printHelpOn(System.err);
        } catch(IOException e) {
            e.printStackTrace();
        }

        System.exit(1);
    }

    public static <T> T valueOf(OptionSet options, String opt, T defaultValue) {
        if(options.has(opt))
            return (T) options.valueOf(opt);
        else
            return defaultValue;
    }
    
    protected OptionSet parse(String[] args) {
        try {
            OptionSet options = parser.parse(args);

            if(options.has("help"))
                printUsage();

            setLogging(options);
            return options;
        } catch(OptionException e) {
            System.err.println(e.getMessage());
            printUsage();
            return null;
        }
    }

    protected void setLogging(OptionSet options) {
        // "Options are \"debug\", \"info\" (default), \"warn\", \"error\", or \"off\"")
        String levelString = valueOf(options, "logging", "info");

        Level level = null;

        if(levelString.equals("debug"))
            level = Level.DEBUG;
        else if(levelString.equals("info"))
            level = Level.INFO;
        else if(levelString.equals("warn"))
            level = Level.WARN;
        else if(levelString.equals("error"))
            level = Level.ERROR;
        else if(levelString.equals("off"))
            level = Level.OFF;
        else
            printUsage();

        Logger rootLogger = Logger.getRootLogger();
        rootLogger.setLevel(level);

        Enumeration<Logger> e = rootLogger.getLoggerRepository().getCurrentLoggers();

        while(e.hasMoreElements()) {
            Logger logger = e.nextElement();
            logger.setLevel(level);
        }
    }

    protected String getRequiredString(OptionSet options, String argumentName) {
        if(!options.has(argumentName)) {
            System.err.println("Missing required argument " + argumentName);
            printUsage();
        }

        return valueOf(options, argumentName, "");
    }
    
    protected String getOptionalString(OptionSet options, String argumentName) {
        if(!options.has(argumentName)) {
            return null;
        }

        return valueOf(options, argumentName, "");
    }

    protected int getRequiredInt(OptionSet options, String argumentName) {
        if(!options.has(argumentName)) {
            System.err.println("Missing required argument " + argumentName);
            printUsage();
        }

        return valueOf(options, argumentName, 0);
    }

    protected long getRequiredLong(OptionSet options, String argumentName) {
        if(!options.has(argumentName)) {
            System.err.println("Missing required argument " + argumentName);
            printUsage();
        }

        return Long.parseLong(valueOf(options, argumentName, "0"));
    }

    protected File getRequiredInputFile(OptionSet options, String argumentName) {
        String fileName = getRequiredString(options, argumentName);
        File file = new File(fileName);

        if(!file.canRead()) {
            System.err.println("File " + fileName + " cannot be read");
            System.exit(2);
        }

        return file;
    }

    protected File getOptionalInputFile(OptionSet options, String argumentName) {
        String fileName = getOptionalString(options, argumentName);
        if ( fileName == null ) {
        	return null;
        }
        File file = new File(fileName);

        if(!file.canRead()) {
            return null;
        }

        return file;
    }
    
    protected List<File> getRequiredInputFiles(OptionSet options, String argumentName) {
        List<String> fileNames = (List<String>) options.valuesOf(argumentName);
        List<File> returnFileList = new ArrayList<File>();
        for(String name: fileNames) {
            File file = new File(name);
            if(file.canRead()) {
                returnFileList.add(file);
            }
        }

        return returnFileList;
    }

    protected File getInputFile(OptionSet options, String argumentName) {
        if(!options.has(argumentName))
            return null;

        String fileName = valueOf(options, argumentName, "");
        File file = new File(fileName);

        if(!file.canRead()) {
            System.err.println("File " + fileName + " cannot be read");
            System.exit(2);
        }

        return file;
    }

    protected List<HostNamePair> getHostNamesPairsFromFile(File file) {
        Map<String, String> properties = getRequiredPropertiesFile(file);
        List<HostNamePair> hostNamePairs = new ArrayList<HostNamePair>();

        for(Map.Entry<String, String> entry: properties.entrySet()) {
            String externalHostName = entry.getKey();
            String internalHostName = entry.getValue() != null ? entry.getValue()
                                                              : externalHostName;
            hostNamePairs.add(new HostNamePair(externalHostName, internalHostName));
        }

        return hostNamePairs;
    }

    protected Map<String, String> getRequiredPropertiesFile(File file) {
        if(!file.canRead()) {
            System.err.println("File " + file.getAbsolutePath() + " cannot be read");
            System.exit(2);
        }

        Properties properties = new Properties();
        InputStream is = null;

        try {
            is = new FileInputStream(file);
            properties.load(is);
        } catch(IOException e) {
            throw new RuntimeException(e);
        } finally {
        	try {
                if (is != null) {
                    is.close();
                }
            } catch (IOException ioe) {
                // ignore
            }
        }

        Map<String, String> map = new HashMap<String, String>();

        for(Map.Entry<Object, Object> entry: properties.entrySet()) {
            String key = entry.getKey() != null ? entry.getKey().toString() : null;
            String value = entry.getValue() != null ? entry.getValue().toString() : null;

            map.put(key, value);
        }

        return map;
    }

    protected String getRegionUrl(OptionSet options) throws Exception {
        if(options.has("region")) {
            return valueOf(options, "region", RegionInfo.REGIONURL_US_EAST);
        } else {
            return RegionInfo.REGIONURL_US_EAST;
        }
    }

    protected String getAccessId(OptionSet options) throws Exception {
        if(!options.has("accessid") && !options.has("accessidfile")) {
            System.err.println("Missing required argument accessid or accessidfile");
            printUsage();
        } else if(options.has("accessid") && options.has("accessidfile")) {
            System.err.println("Provide either accessid or accessidfile, not both");
            printUsage();
        } else if(options.has("accessid")) {
            return valueOf(options, "accessid", "");
        } else {
            File file = new File(valueOf(options, "accessidfile", ""));
            return readFile(file).trim();
        }

        return null;
    }

    protected String getSecretKey(OptionSet options) throws Exception {
        if(!options.has("secretkey") && !options.has("secretkeyfile")) {
            System.err.println("Missing required argument secretkey or secretkeyfile");
            printUsage();
        } else if(options.has("secretkey") && options.has("secretkeyfile")) {
            System.err.println("Provide either secretkey or secretkeyfile, not both");
            printUsage();
        } else if(options.has("secretkey")) {
            return valueOf(options, "secretkey", "");
        } else {
            File file = new File(valueOf(options, "secretkeyfile", ""));
            return readFile(file).trim();
        }

        return null;
    }

    protected List<Integer> getRequiredListIntegers(OptionSet options, String argumentName)
            throws Exception {
        if(!options.has(argumentName)) {
            System.err.println("Missing required argument " + argumentName);
            printUsage();
        }
        return (List<Integer>) options.valuesOf(argumentName);
    }
    
    protected List<String> getRequiredListString(OptionSet options, String argumentName)
    	throws Exception {
    	if(!options.has(argumentName)) {
    		System.err.println("Missing required argument " + argumentName);
    		printUsage();
    	}
    	return (List<String>) options.valuesOf(argumentName);
    }
    
    private String readFile(File file) throws IOException {
    	BufferedReader reader = null;
         try {
             if (file.exists()) {
                 if (file.isDirectory()) {
                     throw new IOException("File '" + file + "' exists but is a directory");
                 }
                 if (file.canRead() == false) {
                     throw new IOException("File '" + file + "' cannot be read");
                 }
             } else {
                 throw new FileNotFoundException("File '" + file + "' does not exist");
             }
             
             StringBuffer fileData = new StringBuffer();
             reader = new BufferedReader(new FileReader(file));
             char[] buf = new char[16];
             int numRead=0;
             while((numRead=reader.read(buf)) != -1){
                 String readData = String.valueOf(buf, 0, numRead);
                 fileData.append(readData);
                 buf = new char[16];
             }
             return fileData.toString();
         } finally {
        	 try {
                 if (reader != null) {
                     reader.close();
                 }
             } catch (IOException ioe) {
                 // ignore
             }
         }
    }
}
