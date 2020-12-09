#ifdef QT_QML_DEBUG
#include <QtQuick>
#endif

#include <ecl/ecl.h>
#include <eql5/eql.h>
#include <eql5/eql_fun.h>
#include <sailfishapp.h>
#include <QGuiApplication>
#include <QQuickView>
#include <QTextCodec>

extern "C" void init_lib_APP__ALL_SYSTEMS (cl_object);

int main(int argc, char *argv[])
{
  // SailfishApp::main() will display "qml/pusfofefe.qml", if you need more
  // control over initialization, you can use:
  //
  //   - SailfishApp::application(int, char *[]) to get the QGuiApplication *
  //   - SailfishApp::createView() to get a new QQuickView * instance
  //   - SailfishApp::pathTo(QString) to get a QUrl to a resource file
  //   - SailfishApp::pathToMainQml() to get a QUrl to the main QML file
  //
  // To display the view, call "show()" (will show fullscreen on device).

  QScopedPointer<QGuiApplication> app(SailfishApp::application(argc, argv));
  QScopedPointer<QQuickView> view(SailfishApp::createView());
  QUrl mainQml = SailfishApp::pathToMainQml();
  QUrl resRoot = SailfishApp::pathTo("");

  QTextCodec* utf8 = QTextCodec::codecForName("UTF-8");
  QTextCodec::setCodecForLocale(utf8);

  EQL eql;
  eql.exec(init_lib_APP__ALL_SYSTEMS);
  eql_fun("app/qml:ini-lib", Q_ARG(QQuickView*, view.data()),
          Q_ARG(const char*, mainQml.toLocalFile().toLatin1().constData()),
          Q_ARG(const char*, resRoot.toLocalFile().toLatin1().constData()));
  EQL::eval("(app:start)");
}
