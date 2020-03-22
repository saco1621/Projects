<!DOCTYPE html>
<html>
        <title>Rickaby Transportt.</title>
        <meta charset="utf-8" />
        <script src="<?php echo get_bloginfo('template_directory'); ?>/main.js"></script>

        <link href="<?php echo get_bloginfo('template_directory'); ?>/style.css" rel="stylesheet">
        <?php  wp_head(); ?>
    </head>
    <body>
        <header>
			<nav>
                <!---here the logo is Rickaby Transport and is linked to home page as required for the assignment-->        
            <p id="logo"><a style="text-decoration:none" href="<?php echo site_url('index.php/home'); ?>">Rickaby Transport</a></p>

				<ul>
                    <!--menu part-->
                <?php wp_nav_menu( array( 'theme_location' => 'page-menu' ) ); ?>
				</ul>
			</nav>
		</header> 
        
     
    
   