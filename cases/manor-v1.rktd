;; 庄园书房案 —— v1 地图版（spec-v1.md）。时间写作 HHMM。
;; 路径段 (节点 进入 离开) 为闭区间；相邻段之间按边权在路上。
((title "庄园书房案（地图版）")
 (briefing "被害人约 20:10 在书房遇害。三名在场者：管家、厨师、园丁。找出凶手。")
 (window 1930 2100)
 (persons (E1 "管家") (E2 "厨师") (E3 "园丁"))
 (culprit E1)
 ;; 作案地点、案发窗口、作案至少需要的连续分钟数
 (crime study 2005 2020 5)

 (map (node kitchen room "厨房")
      (node hall corridor "走廊")
      (node study room "书房")
      (node garden outdoor "花园")
      (edge kitchen hall 1)
      (edge hall study 1)
      (edge hall garden 2)
      ;; 厨房与花园之间有窗，互相看得见
      (window kitchen garden)
      (window garden kitchen))

 (paths
  (path E1 (kitchen 1930 1959) (hall 2000 2003) (study 2004 2018) (hall 2019 2023) (kitchen 2024 2100))
  (path E2 (kitchen 1930 2100))
  (path E3 (garden 1930 1957) (hall 1959 2009) (garden 2011 2100)))

 ;; 物证：精确事实
 (evidence
  (EV1 "走廊里的烟头（园丁常抽的牌子）" (at E3 hall 2000 2005))))
