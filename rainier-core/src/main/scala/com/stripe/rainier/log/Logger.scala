package com.stripe.rainier.log

import com.google.common.flogger.{FluentLogger, LoggerConfig}
import FluentLogger.Api
import java.util.logging.Level

abstract class Logger {
  def logger: FluentLogger
  def setLevel(level: Level) =
    LoggerConfig.of(logger).setLevel(level)

  def SEVERE: Api = logger.at(Level.SEVERE)
  def WARNING: Api = logger.at(Level.WARNING)
  def INFO: Api = logger.at(Level.INFO)
  def FINE: Api = logger.at(Level.FINE)
  def FINER: Api = logger.at(Level.FINER)
  def FINEST: Api = logger.at(Level.FINEST)
}