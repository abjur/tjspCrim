from pyvirtualdisplay import Display
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
import selenium.webdriver.support.ui as ui
import time
import requests
import os

print 'efetuando login...',
display = Display(visible=0, size=(800, 600))
aux = display.start()
url_login = 'https://esaj.tjsp.jus.br/sajcas/login'
chromedriver = "chromedriver"
os.environ["webdriver.chrome.driver"] = chromedriver
driver = webdriver.Chrome(chromedriver)
wait = ui.WebDriverWait(driver, 20)
driver.get(url_login)
wait.until(lambda driver: driver.find_element_by_id('pbEntrar'))
driver.find_element_by_id('usernameForm').send_keys(login)
driver.find_element_by_id('passwordForm').send_keys(senha)
driver.find_element_by_id('pbEntrar').click()
time.sleep(2)
driver.get('https://esaj.tjsp.jus.br/cpo/pg/open.do?gateway=true')
time.sleep(2)
print 'login realizado!'

def pega_peticao(cd_processo, n_processo, path):
  if driver is None:
    print 'vish'
    return
  print 'baixando peticao do processo %s...' % (n_processo),
  urf1 = 'https://esaj.tjsp.jus.br/cpo/pg/autorizarAcessoRecursoProcessoParaUsuario.do?'
  urf2 = 'processoPK.cdProcesso=%s&processoPK.tpOrigemProcesso=2&processoPK.flOri' % (cd_processo)
  urf3 = 'gem=P&processoPK.aliasDaBase=PG5REG&numeroProcesso=%s&origemRecurso=P' % (n_processo)
  urf = urf1 + urf2 + urf3 + '&urlAcessoRecurso=%23'
  driver.get(urf)
  url_processo = driver.find_element_by_tag_name('body').text
  driver.get(url_processo)
  a = driver.find_element_by_xpath('//a[contains(@parametros, "Peti%E7%E3o")]')
  url_peticao = 'https://esaj.tjsp.jus.br/pastadigital/getPDF.action?' + a.get_attribute('parametros')
  driver.get(url_peticao)
  cookies = {}
  for item in driver.get_cookies():
    cookies[item['name']] = item['value']
  r_peticao = requests.get(url_peticao, verify = False, cookies = cookies)
  f = file('%s/%s.pdf' % (path, n_processo), 'wb')
  f.write(r_peticao.content)
  f.close()
  print 'download finalizado!'

def pega_todos_arquivos(cd_processo, n_processo, path):
  if driver is None:
    print 'vish'
    return
  print 'baixando peticao do processo %s...' % (n_processo),
  urf1 = 'https://esaj.tjsp.jus.br/cpo/pg/autorizarAcessoRecursoProcessoParaUsuario.do?'
  urf2 = 'processoPK.cdProcesso=%s&processoPK.tpOrigemProcesso=2&processoPK.flOri' % (cd_processo)
  urf3 = 'gem=P&processoPK.aliasDaBase=PG5REG&numeroProcesso=%s&origemRecurso=P' % (n_processo)
  urf = urf1 + urf2 + urf3 + '&urlAcessoRecurso=%23'
  driver.get(urf)
  url_processo = driver.find_element_by_tag_name('body').text
  driver.get(url_processo)

  all_a = driver.find_elements_by_xpath('//a[@onclick="mostrarPagina(this)"]')
  urls = [a.get_attribute('parametros') for a in all_a]
  urls = [('https://esaj.tjsp.jus.br/pastadigital/getPDF.action?' + u) for u in urls]

  if not os.path.isdir('%s/%s' % (path, n_processo)):
    os.makedirs('%s/%s' % (path, n_processo))

  f = file('%s/%s/arqs.txt' % (path, n_processo), 'w')
  f.write('\n'.join(urls))
  f.close()

  cookies = {}
  for item in driver.get_cookies():
    cookies[item['name']] = item['value']

  for i in range(len(urls)):
    nome = all_a[i].text.encode('utf-8').replace(' ', '_')
    r_peticao = requests.get(urls[i], verify = False, cookies = cookies)
    f = file('%s/%s/%s.pdf' % (path, n_processo, nome), 'wb')
    f.write(r_peticao.content)
    f.close()


