<?php /* Single-project */ ?>

<?php get_header(); ?>



    <div class="site-wrapper">
   <!--The category part on a single post type
       This part should show project type and skill on a specific single post type--->
        <?php
            if( have_posts() ) {
                while ( have_posts() ){
                    echo "The markup of this content comes from single-project.php <br><br>"; 
                    the_post(); 
                    the_content();
                    //project skill
                    echo the_post_thumbnail('single_large');
                    $skill_terms = get_the_terms( get_the_ID(), 'project-skill' );
                    echo "<h3>Project Skill</h3><br>";
                    if( has_term( '', 'project-skill' ) ) {
                        foreach($skill_terms as $term){
                            echo $term->name.'<br>';
                        }
                    }
                    // Project type here
                    $type_terms = get_the_terms( get_the_ID(), 'project-type' );
                    echo "<br><h3>Project Type</h3><br>";
                    if( has_term( '', 'project-type' ) ) {
                        foreach($type_terms as $term){
                            echo $term->name.'<br><br>';
                        }
                    }
                }
            }
        ?>
    </div>
<?php get_footer(); ?>
