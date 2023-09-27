# multi_gabor_discr

## Code and Data for [No pooling in crowding]
This repository contains the code and data used in [No pooling in crowding].

## Abstract

Crowding is the deleterious effect of flanking objects on target perception. Pooling models suggest that signals from the target and the flankers are averaged. A prototypical example in which averaging is well defined is the averaging of orientation signals of Gabor targets and flankers of varying orientations. Here, we show that averaging accounts failed when the Gabors had identical orientations. Stimuli consisted of 1, 2, 3, 5, or 7 Gabors, randomly presented to the left or right of fixation, with the innermost Gabor presented at 10° eccentricity. Gabors formed two types of contours: ‘snakes’ where Gabors were oriented aligned with their path, and ‘ladders’ where they were oriented perpendicular to their path. In two conditions, the Gabors were presented in radial or tangential arrangements, typically expected to yield strong and weak crowding, respectively. Participants indicated the orientation (left or right) of the Gabor(s). All Gabors had identical orientations on a given trial. Participants in Experiment 1 were not informed about the identical orientations; in Experiment 2 they were informed (prior to the experiment). The pattern of results was the same in both experiments: Performance was similar, with comparably small variance between the different numbers of Gabors, in the tangential ladder and snake and the radial ladder conditions. However, discrimination performance strongly deteriorated with increasing numbers of Gabors in the radial snake condition. Interestingly, in both experiments, observers reported that the arrays often appeared to consist of Gabors with varying orientations. Our results show that orientation signals in the radially arranged snakes were not averaged, but instead subject to deleterious spatial interactions that cannot be accounted for by pooling. Using stimuli ideally suited to show pooling, our findings add to the increasing number of ‘special cases’ in which pooling models fail, challenging their ability to account for spatial interactions in vision.

# Files and Directories

## This repository is structured as follows:

* data/: A directory containing the datasets used in the paper, each subfolder for one experiment
    * ms2_uniform_prolific_1_data/

* src/: A directory containing all the analysis code
    * analysis/
    * common/: functions used in preprocessing
    * constant/: constants used in preprocessing
    * plot/: plots for visualization and plots used in the manuscript.
    * preprocess/: code to preprocess raw data

* README.md: This file.

## run experiments

###  Exp 1 and Exp 2
Only difference in instruction:
Exp1: what's the orientation of the item set
Exp2: what's the orientation of the innermost item

* /run_gabor_staircase_practice/gabor_discri_staircase_practice.psyexp
* /run_gabor_staircase/gabor_discri_staircase1.psyexp

###  Exp 3: two tasks
What's the orientation of the innermost item?
Do all items have the same orientation?

* /run_gabor_2_tasks_practice/gabor_discri_staircase_practice.psyexp
* /run_gabor_2_tasks_order2/gabor_2_tasks_order2.psyexp

###  Exp 4: two tasks - colored Gabor
Identical to Exp3 except for the stimuli used in Exp4 were colored Gabors. The innermost Gabor and the rest Gabors have different colors (red and green).
* /run_colored_gabor/gabor_color.psyexp


###  Exp 5: two tasks - adjust Gabor orientation
Participants need to adjust each Gabor's orientation in the response phase. 3 Gabors per trial.

## Licensing

This repository is released under the BSD 2-Clause License.

You are free to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the software and data contained in this repository, subject to the following conditions:

Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

* The software and data are provided "as is", without warranty of any kind, express or implied, including but not limited to the warranties of merchantability, fitness for a particular purpose and noninfringement. In no event shall the authors or copyright holders be liable for any claim, damages or other liability, whether in an action of contract, tort or otherwise, arising from, out of or in connection with the software or data or the use or other dealings in the software or data.

### Contact

Miao Li: chuoli223@hotmail.com

