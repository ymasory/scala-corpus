package org.apache.maven.artifact.ant;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.factory.ArtifactFactory;
import org.apache.maven.artifact.metadata.ArtifactMetadataSource;
import org.apache.maven.artifact.repository.ArtifactRepository;
import org.apache.maven.artifact.resolver.ArtifactNotFoundException;
import org.apache.maven.artifact.resolver.ArtifactResolutionException;
import org.apache.maven.artifact.resolver.ArtifactResolutionResult;
import org.apache.maven.artifact.resolver.ArtifactResolver;
import org.apache.maven.artifact.resolver.filter.AndArtifactFilter;
import org.apache.maven.artifact.resolver.filter.ArtifactFilter;
import org.apache.maven.artifact.resolver.filter.ScopeArtifactFilter;
import org.apache.maven.project.artifact.InvalidDependencyVersionException;
import org.apache.maven.project.artifact.MavenMetadataSource;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Path;
import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set; 


public class MavenDepsTask extends AbstractArtifactWithRepositoryTask{

	public List<File> deps = new ArrayList<File>();

	private List dependencies = new ArrayList();
  
	/**
	 * The id of the path object containing a list of all dependencies.
	 */
	private String pathId;
  
	/**
	 * The id of the fileset object containing a list of all dependencies.
	 */
	private String filesetId;
  
	/**
	 * The id of the object containing a list of all artifact versions.
	 * This is used for things like removing the version from the dependency filenames.
	 */
	private String versionsId;
  
	/**
	 * A specific maven scope used to determine which dependencies are resolved.
	 * This takes only a single scope and uses the standard maven ScopeArtifactFilter.
	 */
	private String useScope;
  
	/**
	 * A comma separated list of dependency scopes to include, in the resulting path and fileset.
	 */
	private String scopes;
  
	/**
	 * A comma separated list of dependency types to include in the resulting set of artifacts.
	 */
	private String type;
  
	/**
	 * Main task execution. Called by parent execute().
	 */
	protected void doExecute(){
    
		if ( useScope != null && scopes != null ){
			throw new BuildException( "You cannot specify both useScope and scopes in the dependencies task." );
		}
    
		if ( getPom() != null && !this.dependencies.isEmpty() ){
			throw new BuildException( "You cannot specify both dependencies and a pom in the dependencies task" );
		}
    
		ArtifactRepository localRepo = createLocalArtifactRepository();
		log( "Using local repository: " + localRepo.getBasedir(), Project.MSG_VERBOSE );
    
		// Look up required resources from the plexus container
		ArtifactResolver resolver = (ArtifactResolver) lookup( ArtifactResolver.ROLE );
		ArtifactFactory artifactFactory = (ArtifactFactory) lookup( ArtifactFactory.ROLE );
		MavenMetadataSource metadataSource = (MavenMetadataSource) lookup( ArtifactMetadataSource.ROLE );
    
		Pom pom = initializePom( localRepo );
		if ( pom != null ){
			dependencies = pom.getDependencies();
		}
		else
			{
				// we have to have some sort of Pom object in order to satisfy the requirements for building the
				// originating Artifact below...
				pom = createDummyPom( localRepo );
			}
    
		if ( dependencies.isEmpty() ){
			log( "There were no dependencies specified", Project.MSG_WARN );
		}
    
		log( "Resolving dependencies...", Project.MSG_VERBOSE );
    
		ArtifactResolutionResult result;
		Set artifacts;
    
		List remoteArtifactRepositories = createRemoteArtifactRepositories( pom.getRepositories() );
    
		try
			{
				artifacts = MavenMetadataSource.createArtifacts( artifactFactory, dependencies, null, null, null );
      
				Artifact pomArtifact = artifactFactory.createBuildArtifact( pom.getGroupId(), pom.getArtifactId(),
																			pom.getVersion(), pom.getPackaging() );
      
				List listeners = Collections.singletonList( new AntResolutionListener( getProject() ) );
      
				Map managedDependencies = pom.getMavenProject().getManagedVersionMap();
      
				ArtifactFilter filter = null;
				if ( useScope != null )
					{
						filter = new ScopeArtifactFilter( useScope );
					}
				if ( scopes != null )
					{
						filter = new SpecificScopesArtifactFilter( scopes );
					}
				if ( type != null )
					{
						ArtifactFilter typeArtifactFilter = new TypesArtifactFilter( type );
						if ( filter != null )
							{
								AndArtifactFilter andFilter = new AndArtifactFilter();
								andFilter.add( filter );
								andFilter.add( typeArtifactFilter );
								filter = andFilter;
							}
						else
							{
								filter = typeArtifactFilter;
							}
					}
      
				result = resolver.resolveTransitively( artifacts, pomArtifact, managedDependencies, localRepo,
													   remoteArtifactRepositories, metadataSource, filter, listeners );
			}
		catch ( ArtifactResolutionException e )
			{
				throw new BuildException( "Unable to resolve artifact: " + e.getMessage(), e );
			}
		catch ( ArtifactNotFoundException e )
			{
				throw new BuildException( "Dependency not found: " + e.getMessage(), e );
			}
		catch ( InvalidDependencyVersionException e )
			{
				throw new BuildException( "Invalid dependency version: " + e.getMessage(), e );
			}
    
		Path dependencyPath = new Path(getProject());
		Set<String> versions = new HashSet<String>();
    
		for (Iterator i = result.getArtifacts().iterator(); i.hasNext(); ){
			Artifact artifact = (Artifact) i.next();
			addArtifactToResult( localRepo, artifact, dependencyPath );
			versions.add( artifact.getVersion() );
		}
    

	}
  
	private void addArtifactToResult( ArtifactRepository localRepo, Artifact artifact, Path path ){
		File baseDir = new File(localRepo.getBasedir());
		if(baseDir.exists()){
			String filename = localRepo.pathOf(artifact);
			File file = new File(baseDir, filename);
			System.out.println(file);
			if(file.exists()){
				deps.add(file);
			}
		}
	}
  
  
	/**
	 * Use the maven artifact filtering for a particular scope. This
	 * uses the standard maven ScopeArtifactFilter.
	 *
	 * @param useScope
	 */
	public void setUseScope( String useScope )
	{
		this.useScope = useScope;
	}
  
	public void setType( String type )
	{
		this.type = type;
	}
  
	public String getScopes()
	{
		return scopes;
	}
  
	/**
	 * Only include artifacts that fall under one of the specified scopes.
	 *
	 * @return
	 */
	public void setScopes( String scopes )
	{
		this.scopes = scopes;
	}
  

} 
