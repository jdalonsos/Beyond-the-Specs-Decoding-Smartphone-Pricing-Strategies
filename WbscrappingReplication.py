import time
from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import TimeoutException, NoSuchElementException

# Setup WebDriver
chromedriver = "C:/chromedriver-win64/chromedriver.exe"
options = webdriver.ChromeOptions()
options.binary_location = "C:/Program Files/BraveSoftware/Brave-Browser/Application/brave.exe"
driver = webdriver.Chrome(service=Service(chromedriver), options=options)

driver.get('https://www.m*s*ar*p*i*e.com/')
driver.maximize_window()

def click_element(xpath, delay=5):
    """Clicks an element after waiting for it to be clickable."""
    try:
        element = WebDriverWait(driver, delay).until(EC.element_to_be_clickable((By.XPATH, xpath)))
        element.click()
    except (TimeoutException, NoSuchElementException):
        print(f"Element {xpath} not found.")

# Navigate to mobiles and all phones
click_element('//*[@id="Banner"]/aside/div/div/a[1]/div[2]')
time.sleep(2)
click_element('/html/body/div[4]/div/div[3]/div/div/div[2]/div[1]/div[1]/div[2]/a')

time.sleep(5)
# Set price filter
try:
    price_input = WebDriverWait(driver, 5).until(
        EC.presence_of_element_located((By.XPATH, '//*[@id="content-section-bottom"]/div[2]/div[2]/div[3]/div[2]/div[2]/div[1]/input'))
    )
    price_input.clear()
    price_input.send_keys('5000', Keys.ENTER)
except (TimeoutException, NoSuchElementException):
    print("Price input field not found.")

count = 1
while True:
    time.sleep(5)
    with open("web_content.html", "a", encoding="utf-8") as file:
        file.write(driver.page_source)
    print(f"Page {count} data saved.")
    
    try:
        next_button = WebDriverWait(driver, 10).until(
            EC.element_to_be_clickable((By.XPATH, '//a[contains(text(), "Next") and contains(@class, "pgntn__item")]'))
        )
        next_button.click()
        count += 1
    except (TimeoutException, NoSuchElementException):
        print("No more pages to scrape.")
        break

driver.quit()
