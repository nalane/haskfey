module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Lib

main :: IO ()
main = defaultMain $ 
    testGroup "Matrix function tests"
        [identityTest, translateTest, multiplyTest, dotTest, subTest]

identityTest :: TestTree
identityTest = testCase "Testing Identity Matrix" $
    assertEqual "Should be the 4x4 identity matrix" idTest identity where
        idTest = [
            [1, 0, 0, 0],
            [0, 1, 0, 0],
            [0, 0, 1, 0],
            [0, 0, 0, 1]] 

translateTest :: TestTree
translateTest = testCase "Testing Translation Matrix" $ 
    assertEqual "Sould translate to (5, 6, 7)" trTest $
    translate [5, 6, 7] where
        trTest = [
            [1, 0, 0, 5],
            [0, 1, 0, 6],
            [0, 0, 1, 7],
            [0, 0, 0, 1]] 

multiplyTest :: TestTree
multiplyTest = testGroup "Testing Multiplication of Matrices" [
    testCase "Two Generic Matrices" $
        assertEqual "Multiplying generic matrices failed" muTest $
        multiply aMat bMat,
    testCase "Identity Matrix" $
        assertEqual "Multiplying generic and identity matrices failed" aMat $
        multiply aMat identity,
    testCase "Zero Matrix" $
        assertEqual "Multiplying generic and zero matrices failed" zeroMat $
        multiply aMat zeroMat] where
            aMat = [
                [1, 2, 3, 4],
                [5, 6, 7, 8],
                [9, 10, 11, 12],
                [13, 14, 15, 16]]
            bMat = [
                [17, 18, 19, 20],
                [21, 22, 23, 24],
                [25, 26, 27, 28],
                [29, 30, 31, 32]]
            muTest = [
                [250, 260, 270, 280],
                [618, 644, 670, 696],
                [986, 1028, 1070, 1112],
                [1354, 1412, 1470, 1528]]
            zeroMat = replicate 4 $ replicate 4 0

dotTest :: TestTree
dotTest = testGroup "Testing Dot Product of Two 3D Vectors" [
    testCase "Two Generic Vectors" $
        assertEqual "Dotting two generic vectors failed" 32 $
        dot aVec bVec,
    testCase "Zero vector" $
        assertEqual "Dotting generic and zero vectors failed" 0 $
        dot aVec zVec,
    testCase "Normal vectors" $
        assertEqual "Dotting two normal vectors failed" 0 $
        dot xVec yVec] where
            aVec = [1, 2, 3]
            bVec = [4, 5, 6]
            zVec = [0, 0, 0]
            xVec = [1, 0, 0]
            yVec = [0, 1, 0]

subTest :: TestTree
subTest = testCase "Testing Subtraction of Two 3D Vectors" $
    assertEqual "Subtracting one vector from another failed" [1, -2, -3] $
    sub [4, -4, -1] [3, -2, 2]