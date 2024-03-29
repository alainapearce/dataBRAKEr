# This script was written by Alaina Pearce in 2020
# to generate energy density databases to calculate
# caloric intake for the keller lab
#
#     Copyright (C) 2021 Alaina L Pearce
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <https://www.gnu.org/licenses/>.


#### Study BRAKE ####

brake_ed <- data.frame(meal = c(rep('meal', 6), rep('eah', 10)),
                     food = c("chkn_nug", "mac_cheese", "grapes", "carrot", "ketchup", "water", "brownies", "cornchips", "hersheys", "icecream", "oreos", "popcorn", "pretzels", "skittles", "starbursts", "water"),
                     ed = c(2.500, 1.7, 0.695, 0.417, 1.167, 0, 4.364, 5.707, 5, 1.875, 4.712, 5.667, 3.923, 4, 4, 0))

#write out to raw-data
write.csv(brake_ed, "data-raw/brake_food-ed_ref.csv", row.names = FALSE)

#make a database for the package
usethis::use_data(brake_ed, overwrite = TRUE)

