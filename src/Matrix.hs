{- |Contains functions for creating and manipulating matrices for 3D rendering
-}

module Matrix (
    Vec3, FeyMatrix(..), identity, translate, scale, rotate,
    camera, orthographic,
    multiply, dot, sub, cross, normalize
) where

import Graphics.Rendering.OpenGL (GLfloat)

type Vec3 = [GLfloat]
type Vec4 = [GLfloat]

-- |Fey implementation of a 4x4 matrix for rendering
type FeyMatrix = [[GLfloat]]

-- |Simple 4x4 identity matrix
identity :: FeyMatrix
identity = [
    [1, 0, 0, 0],
    [0, 1, 0, 0],
    [0, 0, 1, 0],
    [0, 0, 0, 1]]   
    
-- Model matrices

-- |Creates a matrix that translates a vertex to the point given
translate :: Vec3 -> FeyMatrix
translate (x:y:z:_) = [
    [1, 0, 0, x],
    [0, 1, 0, y],
    [0, 0, 1, z],
    [0, 0, 0, 1]]

scale :: Float -> FeyMatrix
scale s = [
    [s, 0, 0, 0],
    [0, s, 0, 0],
    [0, 0, s, 0],
    [0, 0, 0, 1]]

-- |Creates a metrix to rotate by theta degrees about the given axis
rotate :: Float -> Vec3 -> FeyMatrix
rotate theta axis = [
    [xx * invcost + cost, xy * invcost - (z * sint), xz * invcost + (y * sint), 0],
    [xy * invcost + (z * sint), yy * invcost + cost, yz * invcost - (x * sint), 0],
    [xz * invcost - (y * sint), yz * invcost + (x * sint), zz * invcost + cost, 0],
    [0, 0, 0, 1]] where
        (x:y:z:_) = normalize axis
        xx = x * x
        xy = x * y
        xz = x * z
        yy = y * y
        yz = y * z
        zz = z * z
        cost = cos theta
        sint = sin theta
        invcost = 1 - cost
        invsint = 1 - sint

-- View Matrices

-- |Creates a camera which exists at pos, is pointed at lookAt
-- and is oriented with upVector
camera :: Vec3 -> Vec3 -> Vec3 -> FeyMatrix
camera pos lookAt upVector = [
    [lx, ly, lz, -(dot left pos)],
    [ux, uy, uz, -(dot normUp pos)],
    [fx, fy, fz, -(dot forward pos)],
    [0, 0, 0, 1]] where
        forward = normalize $ sub pos lookAt
        left = normalize $ cross upVector forward
        normUp = cross forward left
        (fx:fy:fz:_) = forward
        (lx:ly:lz:_) = left
        (ux:uy:uz:_) = normUp

-- |Creates an orthographic projection matrix
orthographic :: Int -> Int -> FeyMatrix
orthographic width height = [
    [1/r, 0, 0, 0],
    [0, 1/t, 0, 0],
    [0, 0, -1, 0],
    [0, 0, 0, 1]] where
        t 
            | width > height = fromIntegral height / fromIntegral width
            | otherwise = 1
        r 
            | width > height = 1
            | otherwise = fromIntegral width / fromIntegral height

-- Matrix utils

-- |Multiplies two 4x4 matrices
multiply :: FeyMatrix -> FeyMatrix -> FeyMatrix
multiply a b =
    map (\k -> 
        map (\j -> 
            sum $ map (\i ->
                a !! k !! i * (b !! i !! j)
            ) [0, 1, 2, 3]
        ) [0, 1, 2, 3]
    ) [0, 1, 2, 3]

-- Vector utils

-- |Finds the dot product between two vectors
dot :: Vec3 -> Vec3 -> GLfloat
dot a b = sum $ zipWith (*) a b

-- |Subtract one vector from another
sub :: Vec3 -> Vec3 -> Vec3
sub (ax:ay:az:_) (bx:by:bz:_) = [ax - bx, ay - by, az - bz]

-- |Find the cross product of two vectors
cross :: Vec3 -> Vec3 -> Vec3
cross (ax:ay:az:_) (bx:by:bz:_) = [
    ay * bz - (az * by),
    az * bx - (ax * bz),
    ax * by - (ay * bx)]

-- |Make the length of an input vector one
normalize :: Vec3 -> Vec3
normalize (x:y:z:_) = [x/l, y/l, z/l] where
    l = sqrt $ sum $ map (^2) [x, y, z]