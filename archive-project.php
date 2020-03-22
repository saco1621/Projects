
<?php get_header(); ?>
<div class="site-wrapper">
	<div id="main-wrapper">
		<?php
			$args = array( 'post_type' => 'project', 'posts_per_page' => 10 );
			$loop = new WP_Query( $args );
			while ( $loop->have_posts() ) : $loop->the_post();
				echo '<div id="container">';
				echo the_post_thumbnail('grid_thumbnail');
				echo '<h2>';
				echo the_title();
				echo '</h2>';
				echo '<br>';
				echo '<div id="wrapper">';
				echo '<a href="'.get_the_permalink().'">See whole project</a>';
				echo '</div>';
				echo '<br>';
				echo '<br>';
				echo '</div>';
				endwhile;
		?>
	</div>
</div>
<?php get_footer(); ?>