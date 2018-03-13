module SVG_creator where

import List


data SVG_color = SVG_color {red::String, green::String, blue::String}

instance Show SVG_color where
  show c = "#" ++ red c ++ green c ++ blue c

-- data SVG_argument = SVG_arg_num { title :: String , value :: Double}
--                   | SVG_arg_str { title :: String , value :: String}
--                   | SVG_arg_col { title :: String , value :: SVG_color}

-- instance Show SVG_argument where
--   show arg = title arg  ++ "=" ++ singlequote . show . value $ arg

-- svg_arg_stroke = SVG_arg_col "stroke"
-- svg_arg_fill   = SVG_arg_col "fill"
-- svg_arg_strokewidth = SVG_arg_num "stroke-width"

data SVG_style = SVG_stroke SVG_color
               | SVG_fill SVG_color
               | SVG_strokewidth Int

instance Show SVG_style where
  show (SVG_stroke s) = show_arg "stroke" s
  show (SVG_fill s) = show_arg "fill" s
  show (SVG_strokewidth s) = show_arg "stroke-width" s

data SVG_drawable = Circle {cx :: Double , cy :: Double, r :: Double}
                  | Rectangle {x :: Double, y :: Double, rw::Double, rh :: Double}
                  | Line {x1 :: Double, y1 :: Double, x2 :: Double, y2 :: Double}

instance Show SVG_drawable where
  show (Circle {cx=x,cy=y,r=rad}) = "<circle " ++ unwords (zipWith show_arg ["cx","cy","r"] [x,y,rad] ) ++ "/>"
  show (Rectangle {x=x,y=y,rw=rw,rh=rh}) = "<rect " ++ unwords (zipWith show_arg ["x","y","width","height"] [x,y,rw,rh] ) ++ "/>"
  show (Line {x1=x1,y1=y1,x2=x2,y2=y2}) = "<line " ++ unwords (zipWith show_arg ["x1","y1","x2","y2"] [x1,y1,x2,y2] ) ++ "/>"

data SVG_element = SVG_element { element :: SVG_drawable, style :: [SVG_style]}

instance Show SVG_element where
  show (SVG_element {element=e, style=s}) =
    insert_style $ show e
    where insert_style str@(x:xs)
            | beginWith "/>" str = ' ' : (unwords $ map show s) ++ " />"
            | otherwise = x : insert_style xs


data SVG_document = SVG_document {
  width    :: Double,
  height   :: Double,
  contents :: [SVG_element]}

instance Show SVG_document where
  show (SVG_document {width=w,height=h,contents=es}) = unwords ["<svg xmlns='http://www.w3.org/2000/svg'",
                                                                show_arg "width" w,
                                                                show_arg "height" h,
                                                                ">"]
                                                       ++ unlines (map show es ) ++ "</svg>"

-- instance Show SVG_element where
--   show SVG_element {element = c@Circle, style = s} = "<circle "
--                                                    ++ unwords . map show_arg $ map ($c) [cx, xy, r] ++ s
--                     ++ "cy =" ++ singlequote cx c
--                     ++ "r =" ++ singlequote cx c
--                     ++ "cx =" ++ singlequote cx c
--     where sh title value                   


singlequote e = "'" ++ show e ++ "'"
show_arg title value = title ++ "=" ++ singlequote value


svg_parseHexColor str = SVG_color r g b
  where [r,g,b] = splitEach 2 str
