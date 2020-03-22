
<?php get_header();
    $page = get_page_by_title( 'About Homepage' );
    $content = apply_filters('the_content', $page->post_content); 
    echo '<div class="site-wrapper">';
    echo $content;
    $args = array(
        'post_type'              => 'project',
        'post_status'            => 'publish',
        'posts_per_page'         => 3,
    );
    $query = new WP_Query( $args );
    if ( $query->have_posts() ) {
        while ( $query->have_posts() ) {
            $query->the_post();
            echo '<div id="container">';
			echo the_post_thumbnail('grid_thumbnail');
			echo '<h2>';
			echo the_title();
			echo '</h2>';
            echo '<br>';
            //click read more to go to a single post type
			    echo '<div id="wrapper">';
			    echo '<a href="'.get_the_permalink().'">Read More</a>';
			    echo '</div>';
			    echo '<br>';
			    echo '<br>';
			    echo '</div>';
        }
        //if there are no posts, "else{...echos that there are no posts yet..} "
    } else {
        echo "There are no posts yet";
    }
    wp_reset_postdata();
    echo '</div>';

get_footer(); ?>
