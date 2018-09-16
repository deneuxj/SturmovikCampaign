let dmg = 0.2f
let rec battle (attackers, defenders) =
    if defenders <= 0.0f || attackers <= 0.1f then
        attackers
    else
        battle (attackers - dmg * defenders, defenders - dmg * attackers)

for defenders in 0.0f..1.0f..10.0f do
    let res = battle(10.0f, defenders)
    printfn "%0.3f %0.3f" (10.0f - defenders) res
