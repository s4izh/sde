FROM archlinux:latest

ARG USERNAME=sergio
ARG PASSWORD=test
ARG HOSTNAME=test
ARG REPOSITORY=/home/${USERNAME}/.dotfiles

ENV HOME /home/${USERNAME}

RUN pacman -Syu --noconfirm
RUN pacman -S base base-devel --noconfirm

RUN echo ${HOSTNAME} > /etc/hostname
RUN echo 'en_US.UTF-8 UTF-8' >> /etc/locale.gen
RUN locale-gen
RUN export LANG=en_US.UTF-0
RUN echo LANG=en_US.UTF-8 > /etc/locale.conf

RUN useradd -m -r -G wheel -s /bin/bash ${USERNAME}
RUN echo "root:${PASSWORD}" | chpasswd
RUN echo "${USERNAME}:${PASSWORD}" | chpasswd
RUN echo '%wheel ALL=(ALL) ALL' | EDITOR='tee -a' visudo

RUN pacman -Syy
RUN pacman -S zsh git wget --noconfirm
RUN sed -i '/sergio/s#/bin/bash#/usr/bin/zsh#' /etc/passwd
RUN sed -i '/sergio/s#973#1000#' /etc/passwd

ENV HOME /home/${USERNAME}
WORKDIR /home/${USERNAME}

COPY . /home/${USERNAME}/bootstrap
WORKDIR /home/${USERNAME}/bootstrap
RUN archlinux/bootstrap/zsh ${USERNAME} /home/${USERNAME}/bootstrap
RUN chown -R ${USERNAME}:${USERNAME} /home/${USERNAME}
USER ${USERNAME}
