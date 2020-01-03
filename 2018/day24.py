#!/usr/bin/env python3

import re

DEBUG=False

class Group:
    def __init__(self, id_val, line, boost = 0):
        patt = "(?P<unit_count>[0-9]*) units each with (?P<hp>[0-9]*) hit points (?:\((?:weak to (?P<weaklist>[^;)]*))?;? ?(?:immune to (?P<immunelist>[^;)]*))?;? ?(?:weak to (?P<weaklist2>[^;)]*))?\) )?with an attack that does (?P<damage_count>[0-9]*) (?P<damage_type>.*) damage at initiative (?P<initiative_count>[0-9]*)"
        props = re.match(patt, line).groupdict()
        self.unit_count = int(props["unit_count"])
        self.unit_hp = int(props["hp"])
        self.damage_count = int(props["damage_count"]) + boost
        self.damage_type = props["damage_type"]
        self.initiative = int(props["initiative_count"])

        self.id_val = id_val

        if props["weaklist"]:
            self.weaknesses = [s.strip() for s in props["weaklist"].split(",")]
        else:
            self.weaknesses = []

        if props["weaklist2"]:
            weaknesses2 = [s.strip() for s in props["weaklist2"].split(",")]
        else:
            weaknesses2 = []
        self.weaknesses += weaknesses2

        if props["immunelist"]:
            self.immunes = [s.strip() for s in props["immunelist"].split(",")]
        else:
            self.immunes = []

    def __str__(self):
        return self.id_val
        return "{} units w/{} hp, damage {} {}, initiative {}, weaknesses {} "\
                "immunities {}".format(self.unit_count, self.unit_hp,
                        self.damage_count, self.damage_type, self.initiative,
                        self.weaknesses, self.immunes)

    def effective_power(self):
        return self.unit_count * self.damage_count

    def damage_calc(self, enemy):
        default_damage = self.effective_power()
        if self.damage_type in enemy.immunes:
            return 0
        elif self.damage_type in enemy.weaknesses:
            return default_damage * 2
        return default_damage

    def select_enemy(self, enemies, cur_targets):
        cur_tgted = [tgt for _, tgt in cur_targets]
        rem_enem = [enem for enem in enemies if enem not in cur_tgted]
        damages = sorted([(self.damage_calc(enem), enem.effective_power(),
            enem.initiative, enem)
            for enem in rem_enem])
        damages.reverse()
        if damages:
            if damages[0][0] > 0:
                return damages[0][3]
            else:
                return None
        return None

    def take_damage(self, dmg_done):
        self.units_killed = dmg_done // self.unit_hp
        self.unit_count -= self.units_killed
        if self.unit_count < 0:
            self.unit_count = 0


def print_groups(immune_groups, infect_groups):
    for group in immune_groups:
        print(group)
    print("-----------")
    for group in infect_groups:
        print(group)

def target_selection(immune_groups, infect_groups):
    ordering = sorted([(group.effective_power(), group.initiative, group)
        for group in immune_groups + infect_groups])
    ordering.reverse()
    
    if DEBUG: print("\nTARGETING:")
    targets = list()
    for _, _, group in ordering:
        enemies = immune_groups if group in infect_groups else infect_groups
        seld = group.select_enemy(enemies, targets)
        if DEBUG: print("{} targets {}".format(group, seld))
        if seld is not None:
            targets.append((group, seld))

    return targets

def attack_phase(targets, immune_groups, infect_groups):
    ordering = sorted([(group.initiative, group)
        for group in immune_groups + infect_groups])
    ordering.reverse()

    tgt_dict = {attack: defend for attack, defend in targets}

    damage_done = False
    if DEBUG: print("\nATTACK!")
    for _, attack_group in ordering:
        if attack_group.unit_count > 0 and attack_group in tgt_dict:
            tgt_group = tgt_dict[attack_group]
            dmg_done = attack_group.damage_calc(tgt_group)
            pre_unit_count = tgt_group.unit_count
            tgt_group.take_damage(dmg_done)
            if tgt_group.unit_count != pre_unit_count:
                damage_done = True
            if DEBUG: print("{} attacks with {} killing {} {}".format(
                attack_group, dmg_done, pre_unit_count - tgt_group.unit_count, tgt_group))
    return damage_done

def remove_dead(groups):
    return [group for group in groups if group.unit_count > 0]

def run_fights(immune_groups, infect_groups):
    while immune_groups and infect_groups:
        targets = target_selection(immune_groups, infect_groups)
        damage_done = attack_phase(targets, immune_groups, infect_groups)
        if not damage_done:
            break

        immune_groups = remove_dead(immune_groups)
        infect_groups = remove_dead(infect_groups)
    return immune_groups, infect_groups

def run_with_boost(immune_lines, infect_lines, boost):
    immune_groups = [Group("Immune_{}".format(ind+1), line, boost=boost)
            for ind, line in enumerate(immune_lines)]
    infect_groups = [Group("Infect_{}".format(ind+1), line)
            for ind, line in enumerate(infect_lines)]

    if DEBUG: print("Starting")
    if DEBUG: print_groups(immune_groups, infect_groups)

    immune_groups, infect_groups = run_fights(immune_groups, infect_groups)

    if DEBUG: print_groups(immune_groups, infect_groups)

    if immune_groups and infect_groups:
        print("Boost: {} Tie".format(boost))
    elif immune_groups:
        print("Boost: {} Immune: {}".format(boost, sum(grp.unit_count for grp in immune_groups)))
        return True
    elif infect_groups:
        print("Boost: {} Infect: {}".format(boost, sum(grp.unit_count for grp in infect_groups)))
    else:
        print("Boost: {} All dead".format(boost))
    return False

if __name__ == "__main__":
    with open("input day 24.txt") as f:
        lines = [line for line in f]

    immune_sys_start = lines.index("Immune System:\n") + 1
    infections_start = lines.index("Infection:\n") + 1

    immune_lines = lines[immune_sys_start:infections_start-2]
    infect_lines = lines[infections_start:]

    boost = 0
    notwin = True
    while notwin:
        notwin = not run_with_boost(immune_lines, infect_lines, boost)
        boost += 1
