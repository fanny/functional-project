import React, {FC} from 'react'
import { createStyles, makeStyles, Theme } from '@material-ui/core/styles';
import InputLabel from '@material-ui/core/InputLabel';
import MenuItem from '@material-ui/core/MenuItem';
import FormControl from '@material-ui/core/FormControl';
import Select from '@material-ui/core/Select';

type Props = {
    title: string,
    options: Set<number>,
    value: number,
    handleChange: any,
}

const useStyles = makeStyles((theme: Theme) =>
  createStyles({
    formControl: {
      margin: theme.spacing(1),
      minWidth: 120,
    }
  }),
);

const CustomSelect:FC<Props> = ({ title, options, value, handleChange}) => {
    const classes = useStyles({})
    return (
        <div dir="rtl">
          <FormControl className={classes.formControl} >
            <InputLabel id="demo-simple-select-label" htmlFor="grouped-select">{title}</InputLabel>
            <Select
              labelId="demo-simple-select-label"
              id="demo-simple-select"
              value={value}
              onChange={handleChange}
              MenuProps={{
                getContentAnchorEl: null,
                anchorOrigin: {
                  vertical: "bottom",
                  horizontal: "left"
                }
              }}
            >
             {Array.from(options).map(option => <MenuItem value={option}>{option}</MenuItem>)}
            </Select>
          </FormControl>
          </div>
    )   
}

export default CustomSelect