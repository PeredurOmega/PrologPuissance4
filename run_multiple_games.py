import time

from selenium import webdriver
from selenium.webdriver.common.action_chains import ActionChains

PATH = "/usr/bin/chromedriver"
driver = webdriver.Chrome(PATH)
driver.get("http://localhost:8000/game")

driver.implicitly_wait(5)

yellow_win_counter = 0
red_win_counter = 0
draw_counter = 0

start_button = driver.find_element_by_id("control-start")


def findWinner():
    final_message = driver.find_element_by_id("win-message")
    final_message_content = final_message.get_attribute("innerHTML")
    if "égalité" in final_message_content:
        winner_color = "draw"
    else:
        winner_color = str(final_message_content).split("(")[1].split(")")[0]
    return winner_color


for i in range(30):
    player1 = driver.find_element_by_id("4")
    player2 = driver.find_element_by_id("7")
    p1_name = player1.get_attribute("data-name")
    p2_name = player2.get_attribute("data-name")
    rematch_button = driver.find_element_by_id("rematch")

    choose_and_play = ActionChains(driver)
    choose_and_play.click(player1)
    choose_and_play.click(player2)
    choose_and_play.click(start_button)

    rematch = ActionChains(driver)
    rematch.click(rematch_button)

    choose_and_play.perform()
    time.sleep(60)  # Temps nécessaire pour jouer la partie
    winner = findWinner()
    if winner == "jaune":
        yellow_win_counter += 1
    elif winner == "rouge":
        red_win_counter += 1
    elif winner == "draw":
        draw_counter += 1
    nbCoupTotal = len(list(driver.find_elements_by_class_name("board-token")))
    nbCoupRouge = len(list(driver.find_elements_by_class_name("rouge")))
    nbCoupJaune = len(list(driver.find_elements_by_class_name("jaune")))

    time.sleep(1)
    rematch.perform()

    print("Nombre de parties gagnées par les rouges : " + str(red_win_counter))
    print("Nombre de parties gagnées par les jaunes : " + str(yellow_win_counter))
    print("Nombre de parties nulles : " + str(draw_counter))

print("Nombre final de parties gagnées par les rouges : " + str(red_win_counter))
print("Nombre final de parties gagnées par les jaunes : " + str(yellow_win_counter))
print("Nombre final de parties nulles : " + str(draw_counter))
