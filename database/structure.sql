SET SQL_MODE="NO_AUTO_VALUE_ON_ZERO";

--
-- Database: `chessserver`
--

-- --------------------------------------------------------

--
-- Table structure for table `chat`
--

DROP TABLE IF EXISTS `chat`;
CREATE TABLE IF NOT EXISTS `chat` (
  `id` int(10) NOT NULL auto_increment,
  `id_user_from` int(10) NOT NULL,
  `id_user_to` int(10) NOT NULL,
  `date` datetime NOT NULL,
  `message` mediumtext NOT NULL,
  PRIMARY KEY  (`id`)
) ENGINE=MyISAM  DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Table structure for table `gps_positions`
--

DROP TABLE IF EXISTS `gps_positions`;
CREATE TABLE IF NOT EXISTS `gps_positions` (
  `id` int(10) NOT NULL auto_increment,
  `id_user` int(10) NOT NULL,
  `longitude` int(10) NOT NULL,
  `latitude` int(10) NOT NULL,
  `date` datetime NOT NULL,
  PRIMARY KEY  (`id`),
  KEY `id_user` (`id_user`)
) ENGINE=MyISAM  DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Table structure for table `users`
--

DROP TABLE IF EXISTS `users`;
CREATE TABLE IF NOT EXISTS `users` (
  `id` int(11) NOT NULL auto_increment,
  `username` varchar(100) NOT NULL,
  `password` varchar(100) NOT NULL,
  `date_lastlogin` datetime NOT NULL,
  `logged_in` enum('yes','no') NOT NULL default 'no',
  PRIMARY KEY  (`id`),
  KEY `username` (`username`),
  KEY `logged_in` (`logged_in`)
) ENGINE=MyISAM  DEFAULT CHARSET=utf8;

