package com.blogspot.pbrc

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(_) => this
  }
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None

  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
