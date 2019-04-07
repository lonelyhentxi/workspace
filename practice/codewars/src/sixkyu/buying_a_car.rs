pub fn nb_months(old: i32, new: i32, saving: i32, percent: f64) -> (i32, i32) {
    let mut loss_rate = percent /100.0;
    let mut loss_percent = 1f64;
    let mut month = 0;
    let remain =
        |loss_percent:f64,month:i32| f64::from(month * saving) - f64::from(new - old) * loss_percent ;
    while (remain(loss_percent,month).floor() as i32) < 0i32 {
        month += 1;
        loss_percent *= 1f64 - loss_rate;
        if month % 2 == 1 {
            loss_rate += 0.5/100.0;
        }
    }
    (month, remain(loss_percent,month).round() as i32)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn testing(old: i32, new: i32, saving: i32, perc: f64, exp: (i32, i32)) {
        assert_eq!(nb_months(old, new, saving, perc), exp);
    }

    #[test]
    fn basics_nb_months() {
        testing(2000, 8000, 1000, 1.5, (6, 766));
        testing(12000, 8000, 1000, 1.5, (0, 4000));
        testing(8000, 12000, 500, 1.0, (8, 597));
        testing(18000, 32000, 1500, 1.25, (8, 332));
        testing(7500, 32000, 300, 1.55, (25, 122));
    }
}